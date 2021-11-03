% Monads are machines: an intuitive introduction to monads
% Seno
% October, 2021

--------------------------------------------------------------------
**WARNING!!!**
--------------------------------------------------------------------
What follows is full of vague statements meant to COMPLEMENT the actual formal notions with some intuition, hence improving understatement.
These statements ARE NOT what the concepts are in a strict sense and should not be used in any serious argument. 
The readers MUST be comfortable with ill-defined notions appealing to their subjective experience of the world, as well as keep pedantic comments to themselves.
----------------------------------------------------------------------

**REQUISITES:**

* Formally speaking, the reader must know only some basic Haskell, including classes and type and data constructors. Of course, having some experience helps.
* Our main example is an interpreter of a very simple lambda-calculus, but it is very easy to learn.
* We will not use any earlier knowledge of functors nor applicatives.

# Introduction

Most texts introducing monads present no intuition at all. This is related to a way to see mathematics (including computer science as a part of it) as a purely formal matter, and even though when talking to mathematicians they give informal insights on how to think about the things they write, those insights never get to the text. We believe this is foolish, and good intuitions are fundamental, even at the cost of looking like people from the arts department (*argh!*). We still believe that in the end of the day the formal part is what matters, but the informal is very fruitful in the morning, afternoon and even early evening.

Having said that, usually the intuitions presented to explain monads are to vague to capture its essence. Some people talk about "containers", others about "computations". Well, we are mixing both and talk about "[machines](#monads-as-machines)". And we believe this works quite well.

This metaphor came to us after looking different sources to try to have a better grasp of what monads really mean, which led us to, desperately, check the original papers on the subject. Monads were first (at least, explicitly) introduced in computer science by Moggi [@Mog89] in 1989. But not only this is a very technical paper, it uses monads as tool to analyse lambda calculus rather than a tool to write code. This second part came 1990, with a much more accessible paper by Waddler [@Wad90], who also wrote a paper in 1992 [@Wad92] with a second approach to introduce monads for programmers. What we are doing here is basically rewriting part of this latter paper, with few of twists:

* We are introducing our monad as machine metaphor;
* We are adapting the code to the current way Haskell is written;
* We prefer to introduce the Monad class via the *join* function rather than *bind*, but we go back to the [standard way](#def-with-bind) as fast as possible; to this end, we create the [class `Machine`](#monads-as-machines)
* We do not follow the full paper, only the first few sections, which are the ones that explain monads. We also changed the order of the examples presented, because we believe some are more [important](#main-examples) than [others](#more-examples). 

Let's start with an excerpt of that paper's introduction: 

> Say I write an interpreter in a pure functional language.
> 
> To add [error handling](#machine-error) to it, I need to modify the result type to include error values, and at each recursive call to check for and handle errors appropriately. Had I used an impure language with exceptions, no such restructuring would be needed.
>
> To add an [execution count](#machine-count) to it, I need to modify the the result type to include such a count, and modify each recursive call to pass around such counts appropriately. Had I used an impure language with a global variable that could be incremented, no such restructuring would be needed.
> 
> To add an [output instruction](#machine-output) to it, I need to modify the result type to include an output list, and to modify each recursive call to pass around this list appropriately. Had I used an impure language that performed output as a side effect, no such restructuring would be needed.
> 
> Or I could use a *monad*.


# The Abstract Notion of Monad

We will now introduce monads both formally and informally. 

We believe many readers tried to understand monads before and, hence, know a couple of examples. Well, it is better to keep these previously know examples at bay for now, so they don't mess up with the intuitions we are trying to build up in this section. Most of all, the list monad may seem very unrelated, but we will [get back to it](#main-examples).

## The `Machine` class {#monads-as-machines}

As we said above, we chose to differ from the usual way that Haskell does, because we believe this other (well-known) approach makes it easier to understand. To not mess things up and redefine the `Monad` class, and also to reinforce our analogy, we will define a class called `Machine`. We will add an "_" to the name of the functions in this class to avoid conflicts.

```haskell
class Machine m where
  return_ :: a ->  m a
  join_ :: m (m a) -> m a
  fmap_ :: (a -> b) -> m a -> m b
```

The idea behind is the following: 

While an element in `Integer` represents an integer for Haskell, an element in `m Integer` represents an integer written in the machine's code.

The function `return_` simply translates a value in Haskell to the same value written in machine code, e.g., if we take `2 :: Integer`, then `return_ 2 :: m Integer` is the way the machine represents the integer `2`. 

Of course, this works for any of Haskell's types, not only `Integer`. For every type `a`, there is a type `m a` that consists in the way the machine represents the elements of `a`. Since `m a` is itself an type in Haskell, there is an type `m (m a)`. The values with this type are "machine codes written in machine code". Of course, this should be redundant: the machine should be able to emulate itself. And that is what the function `join_` does.

Finally, Haskell functions can be used to handle machine code. Given a function `f` with signature `a -> b`, there is a corresponding function that transform machine codes representing values of type `a` to machine codes representing values of type `b`.

We will refer to a value with type `m a` as an *monadic value* for short. Something important that we need to say is that, in general, not every monadic value can be constructed using `return_`. An element of type `m Integer`, for instance, is something that is a integer for the standards of the machine `m`. It could be something like "whatever integer is in this position of the memory when I read it," or "an integer that will be typed by the user," or "some computation that, if nothing goes wrong, will end up being an integer." We can think that the `return_ 2` means something like "the most basic representation the machine can have for the integer 2", or "the pure integer 2" or even "the constant integer 2."

Having said all that, there are also laws that we want the functions in the `Machine` class to follow. For example, one would expect that if we have a value `x` with type `m a` for some `a` (these are called *monadic values*), then `join (return x) = x` (in other words, that if we take something already in machine code and simply write it in machine code again, well, the machine can ignore this second layer). As another example, now for any value `x :: a` and any function `f :: a -> b` , one would expect that `fmap f (return x)` should be the same as `return (f x)` (performing a computation out of the machine, if possible, is compatible with performing it inside). We will postpone listing all the laws for another moment, to avoid losing our focus (see [this link](http://xenon.stanford.edu/~hwatheod/monads.html) for now).

## Monadic functions and bind

From what we just said, it is not surprising that, in general, one cannot read a what it is inside an `m a` and put it back in the vanilla type `a`. Once a value enters the machine, it will be in the machine forever (well, there are exceptions, but that is usually the case). In the examples we will show below, there is a `show` function with signature `show :: m a -> String`, so at least we can see what is going on, but not even this is necessary.

Most of the time, we communicate with a machine using functions that read "vanilla" values but produce values in "machine code", e.g., functions with signature `a -> m b` or `a -> b -> m c` in opposition to functions with signature `a -> b` or `a -> b -> c`. We will call these *monadic functions*. These are easy to write, since, again, we know how to put vanilla values into the machine but not the reverse.

(Formally speaking, we can define a *monadic function signature* to be any type `a -> x` where x is either of the form `m b` or it is, recursively, a monadic function signature.)

A very common situation is when we have a monadic value with type `m a`, for some `a` and want to pass it to a monadic function of type `a -> m b`. How could we do it? Well, we can't remove the `a` from inside the `m a`, but we can use the ability of the machine to "emulate itself!" The strategy is the following: we first use the `fmap` on our monadic function to get something with type `m a -> m (m b)`; we then it apply to our monadic value and get something with type `m (m b)`; finally, we resolve the redundancy of having an extra `m` by applying `join`. In other words, we use:

```haskell
(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
m_val >>= m_func = join (fmap m_func m_val)
```

This function is called *bind*. It is so powerful that we could have used it and `return` to define `join` and `fmap` instead of how we did. Actually, the default definition of the monad class in Haskell, which we avoided above, is using `return` and `(>>=)`! At this point, we could go back and remove the `{-# LANGUAGE NoImplicitPrelude #-}` and the `import Prelude hiding` but, still, we believe would make our machine metaphor more obscure. 

By design, bind doesn't follow the usual function application notation, `f x`, with the function on the left and the value on the right. And,indeed, there is a function `(=<<)` which follows this convention. But `(>>=)` is more common because it follows the order things usually happen: we have a value, then apply a function, get another value, pass it to another function, etc. This sequencing is something that agrees with the way many "machines" actually work.

Another strange thing is this name "bind". The intuition is that, although we can't read the internal value an `m a`, we can "bind its value" to the variable that we pass to the function `a -> m b`. This intuition will make more sense when we work with the *do notation*. But, we had enough general considerations. Let's put our hands on an example.

## Back to reality: how Haskell do it {#def-with-bind}

# Concrete examples

This is call-by-value, for a call-by-name, see [@Wad92, Subsection 2.9].

To avoid confusing technicalities, we will assume we have a constant monad `M`, that we will choose which of the monads we will present below by commenting/uncommenting the obvious lines of code.

## Representing our lambda calculus in Haskell

```haskell

-- The `Name` type consists of the names we can give to our variables.
type Name = String

-- The terms of our simple lambda-calculus are constructed from Ints or free variables by using addition or lambda-abstraction.
data Term = Con Int
          | Var Name
          | Add Term Term
          | Lam Name Term
          | App Term Term

-- Each term is interpreted into a `Value` by a machine (monad) `m`.
data Value = Wrong 
           | Num Int
           | Fun (Value -> M Value)
-- Note that lambda-terms are interpreted as monadic functions `Value -> M Value` and not as functions `Value -> Value`. This is so we can use these functions to interact with the "machine" using bind.

-- If we have free variables, we need to bind them to values before evaluating the term.
type Environment = [(Name, Value)]

-- ## Instances
instance Show Value where
  show Wrong   = "<wrong>"
  show (Num i) = show i
  show (Fun f) = "<function>"

-- ## Functions defining the interpreter
-- All monadic functions, even though we don't need them all to be.

lookup :: Name -> Environment -> M Value
lookup x []        = return Wrong
lookup x ((y,b):e) = if x==y
                      then return b
                      else lookup x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = return $ Num (i+j)
add _ _ = return Wrong

apply :: Value -> Value -> M Value
apply (Fun f) a = f a
apply _ _ = return Wrong

interp :: Term -> Environment -> M Value
interp (Con i) e = return (Num i)
interp (Var x) e = lookup x e
interp (Add u v) e = interp u e >>=
                      (\a -> interp v e >>=
                        \b -> add a b)
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= (\f -> interp u e >>= \a -> apply f a)

test :: Term -> String
test t = show (interp t [])
```

## Main examples {#main-examples}

### Machine 0: Identity

```haskell
type M = Id

newtype Id a = Id a

instance Monad Id where
  return a = Id a
  join (Id (Id a)) = Id a
  fmap f (Id a) = Id (f a)

instance Show a => Show (Id a) where
  show (Id a) = show a
```

### Machine 1: Error Messages {#machine-errors}

```haskell
-- type M = E

data E a = Error String | Success a 

instance Monad E where
  return :: a -> E a
  return a = Success a

  join :: E (E a) -> E a
  join (Success (Success a)) = Success a
  join (Success (Error msg)) = Error msg
  join (Error msg)           = Error msg

  fmap :: (a -> b) -> E a -> E b
  fmap f (Success a) = Success (f a)
  fmap f (Error msg) = Error msg

instance Show a => Show (E a) where
  show (Success a) = show a
  show (Error msg) = "ERROR! " ++ msg
```

Changes to the interpreter:
```haskell
lookup x [] = errorE ("Unbound variable: " ++ x)
add a b = errorE ("Should be numbers: " ++ show a ++ "," ++ show b)
apply f a = errorE ("Should be a function: " ++ show f)
```

### Machine 2: State {#machine-count}

```haskell

newtype StateMachine state a = SM (state -> (a, state))

instance Monad (StateMachine state) where
  return :: a -> StateMachine state a
  return a = SM $ \s -> (a, s)
  
  join :: StateMachine state (StateMachine state a) -> StateMachine state a
  join (SM metaMachine) = SM newMachine
    where
      newMachine s0 = let (SM machine, s1) = metaMachine s0 in machine s1

  fmap :: (a -> b) -> StateMachine state a -> StateMachine state b
  fmap f (SM machine) = SM newMachine
    where
      newMachine s0 = let (a, s1) = machine s0 in (f a, s1)
```

More stuff:

```haskell


-- instance for show only for the case the `state` is taken to be Int

type CountingMachine = StateMachine Int

instance Show a => Show (CountingMachine a) where
  show (SM machine) = let (a, s1) = machine 0 in
                      "Value: " ++ show a ++ "; " ++
                      "Count: " ++ show s1

-- The typing of `tick` makes clear that the value returned is not of interest. It is analogous to the use in an impure language of a function with result type (), indicating that the purpose of the function lies in a side effect.
tick :: CountingMachine ()
tick = SM $ \s -> ((), s+1)

-- If we want to read the current state, we can use:
fetch :: StateMachine state state
fetch = SM machine
  where
    machine s = (s, s)
```

### Machine 3: Output {#machine-example}

### Machine 4: Non-deterministic choice

## More examples {#more-examples}

### Machine 5: Errors with position

### Machine 6: Reverse State

# The monad laws

## With `return`, `join` and `fmap`
## With `bind`; Monad composition