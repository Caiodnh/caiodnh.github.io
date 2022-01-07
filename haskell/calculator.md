% Monads are machines: an intuitive introduction to monads
% Seno
% November, 2021

--------------------------------------------------------------------
**WARNING!!!**
--------------------------------------------------------------------
What follows is full of vague statements meant to COMPLEMENT the actual formal notions with some intuition, hence improving understatement.
These statements ARE NOT what the concepts are in a strict sense and should not be used in any serious argument. 
The readers MUST be comfortable with ill-defined notions appealing to their subjective experience of the world, as well as keep pedantic comments to themselves.
----------------------------------------------------------------------

**REQUISITES:**

* Formally speaking, the reader must know only some basic Haskell, including classes and type and data constructors. Of course, having some experience helps.
* I does not depends on any earlier knowledge of `Functor` nor `Applicative`.

# Introduction

There are plenty of tutorials on monads out there. Yet, we were frustrated trying to follow most of them. There are two reasons for that:

1. Some tutorials follow a philosophy very common in mathematics and computer science: things are what they are in a very formal sense and, *therefore*, that is all that should be presented. Though we agree with the first first part of the sentence, we disagree with the second. Things are defined and used formally, but there are intents and intuitions that go beyond the formal part, and not presenting those can be very prejudicial to the understatement of concept. Of course, intents and intuitions are very subjective and plural by nature, which make things a bit complicated.

2. Other tutorials present some intuition, but a very deficient one. Some present monads as "containers," but this is too vague since functors in general can be seen as containers. Other tutorials say that monads are things to "sequence computations," which is something that happens in naturally in imperative languages but not in functional ones. Again, this does capture the full notion of monad, and is hardly reflected in its definition.

This problems made us look to the original papers that introduced monads in computer science (mathematicians were considering them in different areas since the 1950s, but we didn't go so far). The first paper to cite them explicitly in computer science was written by Moggi, first published in 1989 and then expanded in 1991 @Mog89. But not only this is a rather technical paper we couldn't read, it did not introduced monads as a programming tool: they were introduced as a tool to analyse lambda calculus. Nevertheless, the ideas to use them for programming were latent, as showed by Waddler in 1990 @Wad90, already using Haskell as the programming language to exemplify it. Waddler also wrote a second paper with a different approach to introduce monads to programmers in 1992 @Wad92. What follows is pretty much inspired by @Wad92, but simplified and adapted to the way Haskell is written now-a-days.

Both @Mog89 and @Wad92 use the vague word "computations" to describe what monads are at an intuitive level. We merged this idea with the so common "container" metaphor to make up our own metaphor: [machines](#monads-as-machines). We are not saying it is original, only that we didn't see it before this way. And this metaphor seems to work so well, including as a way to motivate the [monad laws](#monad-laws), that we decided to write this tutorial. Hopefully, this will be a good way for you to understand monads too!

Before going to the actual text, it is worth to say that there are two ways that monads are usually introduced:

1. Using the functions `return` and `bind`;
2. Or using the functions `return`, `join` and `fmap`.

The first approach involves less functions and laws, and it is closer to what is used in day-by-day programming. This is the one modern day Haskell uses, and is the one used in @Wad92.

The second approach is the most commonly used in pure mathematics, and was the one used in @Mog89 and @Wad90. *We decided to use this approach here.* This is because the machine metaphor seems more intuitive following it. This is not a big deal and we go back to the Haskell's way [shortly after](#def-with-bind). We also show how to translate between the two approaches.

Finally, we start with an excerpt from the introduction of @Wad92: 

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

We believe many readers tried to understand monads before and, hence, know a couple of examples. Well, it is better to keep these previously know examples at bay for now, so they don't mess up with the intuitions we are trying to build up in this section. Most of all, we urge the reader to **forget about the list monad**, since it is a common example that seems very unrelated at first. 
We will come [get back to it](#list-monad), so just hang in there.

## The `Machine` class {#monads-as-machines}

As we said above, we chose to differ from the usual way that Haskell does, because we believe this other approach makes it easier to understand. To not mess things up and redefine the `Monad` class, and also to reinforce our metaphor, we will define a new class called `Machine`. The functions in this class already exist in Haskell, so we will add an "M" to their names to avoid conflicts.

```haskell
class Machine m where
  returnM :: a ->  m a
  joinM :: m (m a) -> m a
  fmapM :: (a -> b) -> m a -> m b
```

Okay, let's dissect this.

First, a machine `m` is a type constructor with kind `* -> *`, meaning that it takes a type as input and produces a new type. While, for instance, an `Integer` is what Haskell considers to be an integer, an `m Integer` is what *the machine* `m` considers an integer. We can think that an `m Integer` is an integer written in machine code. Of course, we could have used any type `a` instead of `Integer` and created the type `m a` of "machine codes that symbolize values of type `a`." To distinguish from the usual Haskell values, we say that values with type `m a` are *monadic values*.

Coming back to the `Integer` case for illustration, it is worth mentioning Machines can be far more lenient with out they accept as an integer than Haskell is. In Haskell, an `Integer` is a constant integer, like the number `2`. For a machine, well, an integer could be "a number that changes according to what is being computed," or "something that we expected to be an integer, but it became an error message" or even "an integer that will be typed by the user while the machine is running." In general, elements with type `Integer` are static while elements with type `m Integer` can be seem as dynamic. Because of this, monadic values are also frequently referred to as *monadic actions*.

Now let's go for the functions in the `Machine` class.

The function `returnM` puts an element inside the machine. 
It can also be thought as translating a value in Haskell to the same value but written in machine code, e.g., if we take `2 :: Integer`, then `returnM 2 :: m Integer` is the way the machine represents the integer `2`. By what we said above, there is no hope that every element of type `m a` is obtained by "returning" an element of type `a`. We can think about `returnM` as constructing the "most basic" monadic values; in other words, `returnM (2 :: Integer)` can be interpreted as "the most basic representation the machine can have for the integer 2", or "the pure integer 2" or even "the integer that is constant and equal to 2."

One of the most interesting aspects of any machine is it ability to "emulate itself." As we said above, for any type `a`, we can construct the type `m a`. In particular, we can construct the type `m (m a)`. What does that mean? Well, if `m a` is a machine code representing something with value `a`, then `m (m a)` is a machine code representing a machine code. Of course, this is redundant, and that is why we have the map `joinM :: m (m a) -> m a`. One way to think about it is that an element with type `m (m a)` is a some code with type `m a` that is already in the memory of the machine, and what `joinM` does is use the machine to *run this code* to get something with type `a` instead.

Finally, we have the `fmapM` function. This used to translate regular Haskell functions so they can be used to handle machine code: given a function `f` with signature `a -> b`, we construct the corresponding function that transform machine codes representing values of type `a` to machine codes representing values of type `b`. To exemplify, let us think about the function `f = (+ 1) :: Integer -> Integer` and let us say we have a monadic value that represents "an integer that will be typed by the user while the machine is running," as we said above. Then, `fmapM f` applied to this value is a new monadic value that represents "first read the element that the user types, and then add `1` to it." Note that, in most imperative languages, both monadic values just cited would be *functions*, but in Haskell this is *not*. Their type is `m Integer`, so we reason about then as integers!

Having said all that, there are also laws that we want the functions in the `Machine` class to follow. For example, one would expect that if we have a value `x` with type `m a` for some `a`, then `join (return x) = x` (in other words, that if we take something already in machine code and simply write it in machine code again, well, the machine can ignore this second layer). As another example, now for any value `x :: a` and any function `f :: a -> b` , one would expect that `fmap f (return x)` should be the same as `return (f x)` (performing a computation out of the machine, if possible, is compatible with performing it inside). We will postpone listing all the laws for another moment, to avoid losing our focus (see [this link](http://xenon.stanford.edu/~hwatheod/monads.html) for now).

## Monadic functions and bind {#bind}

Now we will discuss the most common pattern used to program with monads. 

We first define a function that *constructs* monadic values from regular ones. 
In other words, it has a signature that looks like `a -> m b` or `a -> b -> m c`. 
Functions like that are called *monadic functions*. (Formally speaking, we can define a *monadic function signature* to be any type `a -> x` where x is either of the form `m b` or it is, recursively, a monadic function signature.)

To define such a function, we do not need to know how to interpret a code with type `m a` as an element with type `a`. This is something that happens (if it end up happening) inside the machine. All that is need to define a monadic function is how to *program the machine*, so to have a new element in machine code in the end. Maybe this will be done by using `returnM`, maybe it will be done by using some specific tool we have for this particular machine. 

Once we have our monadic function defined, we want to pass to it a monadic value and not a regular one. This is because typically it we want to pass something that was compute by the machine before. But how can we do that?

Okay, so let's say we have a monadic value `m_a` of type `m a` and want to pass it to the monadic function `f` of type `a -> m b`. We can't use `f` directly, but we can use `fmapM f`, which accepts parameters with type `m a`. The problem is that it will produce an element of type `m (m b)`. Well, this is not really a problem, since we can use `joinM` to interpret this last element as one with type `m b`. In other words, we used the following:

```haskell
bindM :: (Machine m) => m a -> (a -> m b) -> m b
bindM m_a f = joinM $ fmapM f m_a
```

We just defined the famous *bind*. It is so powerful that we could have used it and `returnM` to define `joinM` and `fmapM` instead of how we did. And, actually, that is the [default way to define monads in Haskell](#def-with-bind), and the default way to denote it is by `>>=`.

The reader may have noticed that it has the value as first argument and the function as the second, not following the usual notation for function application, which has the function on the left and the value on the write. This is because, in general, our machine has computed the monadic value *before*, and then it passes it to `bindM`. The notation follows this becaus sequencing is something that is important for many machines. If you'd like, we can read `bindM` as "pass the value to the function" rather than "evaluate the function at the value."

### The name "bind"

The name "bind" is a bit of a strange one. Why this name? "Binding" usually refers to saving a value in a variable, and this doesn't seem to be what is happening.

It turns out, there is something that looks like binding going on. To see that, let us talk about `let` briefly first.

The `let` constructor is something that is used for binding, in the way we usually think about it: in `let x = a in expression`, what is done is saying that `x` represents the value of `a` every time it appears in `expression`. 
<!-- Or, in other words, we substitute `x` for `a` every time it appears in `expression`.  -->
And this is the same thing as function application! What we do to compute `let x = a in expression` is, of course, the same as evaluating the function `(\x -> expression)` on the value `a`.

Well, when the function we pass to `bindM` was a lambda abstraction, then the similarity with this way of thinking about `let` becomes clear. 

Let us try to see with a simple example: suppose we have a double in machine code, i.e., an value `m_x` with type `m Double` and we want to pass it to the function `\x -> return (x + 1)`, which has type `Double -> m Double`. We do
```haskell
m_x `bindM` (\x -> returnM (x + 1))
```
which can be thought as "Let `x` be the integer encoded by `m_x`. Add `1` to `x` and return the result to the machine."

Of course, this is a very strange way to think, since it is not what is actually happening. The monadic value `m_x` may not encode a Haskell integer to save in the variable `x`. We said what is actually happening in the [last subsection](#bind). What we are saying now is that we can *pretend* this is what is happening.

<!-- An useful analogy is like we use the number π in symbolic calculations in a math course. We can write expressions like `7 * π  + π^131` and say this is its precise value of the real number, but when we want a *decimal approximation* of this expression we need to change all occurrences of π to an *decimal approximation* of π. In this analogy, the regular values are "real numbers" and the machine code representations are their "decimal approximations." -->

Let's do another example, that is something that we will use in the [next section](#def-with-bind). 
We will define the function `apM`, which corresponds to the function `ap` in `Control.Monad`. 
This function receives an "encoded" function `m_f` and an "encoded" value `m_x` and gives us the "machine code" of the value we would get by applying the function to the value:
```haskell
apM :: (Machine m) => m (a -> b) -> m a -> m b
apM m_f m_x = m_f `bindM` (\f -> m_x `bindM` (\x -> returnM (f x)))
```
Again, this can be read as "Let `f` be the function encoded by `m_f`. Then let `x` be the value encoded by `m_x`. Then compute `f x` and return the resulting value to the machine."

## Back to reality: how Haskell do it {#def-with-bind}

The moment to translate `Machine` to the way that the `Monad` class currently defined in Haskell has finally arrived! It has some layers: for a type constructor to have an instance of `Monad`, it needs an instance of `Applicative`, which in turn needs an instance of `Functor`.

```haskell
instance Machine m => Functor m where
  fmap = fmapM

instance Machine m => Applicative m where
  pure = returnM
  (<*>) = apM
  
instance Machine m => Monad m where
  return = returnM
  (>>=) = bindM
```

The reverse:

```haskell
instance Monad m => Machine m where
  returnM = return
  joinM (mm_x) = mm_x >>= id
  fmapM f m_x = m_x >>= (\x -> return (f x))
``` 

# Examples

We will now present some examples of monads. We use them

To avoid confusing technicalities, we will assume we have a constant monad `M`, that we will choose which of the monads we will present below by commenting/uncommenting the obvious lines of code.

```haskell
data Expr = Num Double
          | Add Expr Expr
          | Mult Expr Expr

eval :: Expr -> M Double
eval (Num Double)       = return Double
eval (Add expr1 expr2)  = eval expr1 >>= (\x -> eval expr2 >>= (\y -> return $ x + y))
eval (Mult expr1 expr2) = eval expr1 >>= (\x -> eval expr2 >>= (\y -> return $ x * y))
```

## Main examples {#main-examples}

### The trivial case: Identity Machine

```haskell
type M = Id

newtype Id a = Id a

instance Machine Id where
  returnM a = Id a
  joinM (Id (Id a)) = Id a
  fmapM f (Id a) = Id (f a)

instance Show a => Show (Id a) where
  show (Id a) = show a
```

### Error Messages {#machine-errors}

```haskell
-- type M = E

data E a = Error String | Success a 

instance Machine E where
  returnM :: a -> E a
  returnM a = Success a

  joinM :: E (E a) -> E a
  joinM (Success (Success a)) = Success a
  joinM (Success (Error msg)) = Error msg
  joinM (Error msg)           = Error msg

  fmapM :: (a -> b) -> E a -> E b
  fmapM f (Success a) = Success (f a)
  fmapM f (Error msg) = Error msg

instance Show a => Show (E a) where
  show (Success a) = show a
  show (Error msg) = "ERROR! " ++ msg
```

Changes:
```haskell
data Expr = ...
          | Div Expr Expr
          | Sqrt Expr

eval (Div expr1 expr2) = eval expr2 >>= \y ->
                           if y == 0 then Error "Can't divide by zero"
                           else eval expr1 >>= \x -> return $ x / y
eval (Sqrt expr)       = eval expr >>= \x ->
                           if x < 0 then Error "Can't take square root of negative numbers"
                           else return $ sqrt x
```

### Global Constants

Different approximations of π.

Modification to data:

```haskell
  data Expr = ...
            | Pi
```
How to do a machine? It must read the chosen approximation of π before computing.

```haskell
data Reader approx a = Reader (approx -> a)
```

In our example, `approx` is `Double`. The machine is `Reader approx`, not just `Reader`.

```haskell
instance Machine approx where
  returnM a = Reader $ \x -> a
  joinM (Reader metaFunc) = Reader newFunc
    where
      newFunc x = metaFunc x x
  fmapM f (Reader func) = Reader newFunc
    where
      newFunc x = f (func x)
```

Note that fmapM is just composition.

Changes to eval:

```haskell
eval Pi = Reader id
```

### State Machine {#machine-count}

Our next machine has a changing state that influence and is influenced by the computations.
Perhaps this is the most fundamental example.

```haskell
newtype StateMachine state a = SM (state -> (a, state))

instance Machine (StateMachine state) where
  returnM :: a -> StateMachine state a
  returnM a = SM $ \s -> (a, s)
  
  joinM :: StateMachine state (StateMachine state a) -> StateMachine state a
  joinM (SM metaMachine) = SM newMachine
    where
      newMachine s0 = let (SM machine, s1) = metaMachine s0 in machine s1

  fmapM :: (a -> b) -> StateMachine state a -> StateMachine state b
  fmapM f (SM machine) = SM newMachine
    where
      newMachine s0 = let (a, s1) = machine s0 in (f a, s1)
```

For our interpreter, we will use as state an `Int` that counts how man times function is applied or an addition is performed. To this end, we will need the following piece of machinery:

```haskell
tick :: StateMachine Int ()
tick = SM $ \s -> ((), s+1)
```

The typing of `tick` makes clear that the value returned is not of interest.
It is analogous to the use in an impure language of a function that returns nothing, indicating that the purpose of the function lies in a side effect.

Then, we add the following changes in our interpreter:

```haskell
eval (Mult expr1 expr2) = tick >>= eval expr1 >>= (\x -> eval expr2 >>= (\y -> return $ x * y))
```

Finally, we only define an instance of `Show` for the case `state` is `Int` (this definition relies in the `FlexibleInstances` addon):

```haskell
instance Show a => Show (StateMachine Int a) where
  show (SM machine) = let (a, s1) = machine 0 in
                      "Value: " ++ show a ++ "; " ++
                      "Count: " ++ show s1
``` 

#### Variation

An interesting variation of our lambda calculus is to be able to refer to hidden state. For this we need to modify our `Term` definition by adding the `Count` data constructor:

```haskell
data Term = ...
          | Count
```

To read the current state, we use the following:

```haskell
fetch :: StateMachine state state
fetch = SM machine
  where
    machine s = (s, s)
```

Finally, we add the following line to `interp`:
```haskell
interp Count e = fetch >>= (\i -> return (Num i))
```

Then the term `Add (Add (Con 1) (Con 2)) Count` will evaluate to 4.

### Machine 3: Output {#machine-example}

### Machine 4: Non-deterministic choice {#list-monad}

## More examples {#more-examples}

### Machine 5: Errors with position

### Machine 6: Reverse State

# The monad laws {#monad-laws}

## With `return`, `join` and `fmap`
## With `bind`; Monad composition