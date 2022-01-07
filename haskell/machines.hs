{-# LANGUAGE NoImplicitPrelude, InstanceSigs, FlexibleInstances #-}

import Prelude hiding (Functor, Monad, fmap, return, (>>=), lookup)
import Control.Monad.State.Lazy (State)

-- **WARNING!!!**
-- What follows is full of vague statements meant to *complement* the actual formal notions with some intuition, hence improving understatement.
-- These statements ARE NOT what the concepts are in a strict sense and should not be used in any serious argument.
-- Also, the readers must be comfortable with ill-defined notions appealing to their subjective experience of the world and keep pedantic comments to themselves.

-- REQUISITES:
-- Formally speaking, the reader must know only some basic Haskell, including classes and type and data constructors. Of course, having some experience helps.
-- We will not use any earlier knowledge of Functors whatsoever. We also believe that previous examples of monads that the reader may know, especially of the list monad, can cause some confusion at first, so keep the mind free of those to start.
-- We will write an interpreter of a very simple lambda-calculus as our main example, but it is very easy to learn.

-- # Explaning the goal of this article

-- This article is because we believe that its is hard to find a good introduction to monads that gives some satisfactory motivation and ituition. As it is usual with advanced math topics, only the mechanical and formal parts are usually introduced. In months studying Haskell, we went to look different sources to try to have a better grasp of the concept, but it felt that there was always something missing.

-- This made us went to look to the original academic papers that introduced the concept. It is generally considered that Moggi89 is the first paper, but it is a very theoretical paper assuming lot's of category theory. Nevertheless, is says in its introduction that monads are used to different forms of "computation", which was not something we understood when read it. 

-- The popularization of the concept came with Waddler, who wrote much more accesible papers, one 1990 and another in 1992. Here, we follow very closely the 1992 one, but with some twists. First, he still use the vague  "computation", which we translated to the (also vague but more concrete) notion of "machine". We believe that this makes things much more clear to our intuition. Second, we prefer to introduce manads via return, join and fmap, instead of return and bind, that is the usual. Of course, since version xx, Haskell actually uses Funtor and Applicative subclasses. We decided that this can bring lots of confusion to start with, so we let to introduce this on a latter moment.

-- Third, some of the examples of monads that were in Wad92 were transfered to a section of more examples, with the goal to focus on the more important ones and avoid getting confused with technicalities.

-- Fourth, we are only showing what are the first few sections of the paper, letting the interested reader to go there to read more about.

-- Fith, some things were changed to the notation closer to what haskell have now-a-days, and not how it was 1992.

-- ## Exerpt from Wad92

-- > Say I write an interpreter in a pure functional language.

-- > To add error handling to it, I need to modify the result type to include error values, and at each recursive call to check for and handle errors appropriately. Had I used an impure language with exceptions, no such restructuring would be needed.

-- > To add an execution count to it, I need to modify the the result type to include such a count, and modify each recursive call to pass around such counts appropriately. Had I used an impure language with a global variable that could be incremented, no such restructuring would be needed.

-- > To add an output instruction to it, I need to modify the result type to include an output list, and to modify each recursive call to pass around this list appropriately. Had I used an impure language that performed output as a side e ect, no such restructuring would be needed.

-- > Or I could use a *monad*.



-- # Monads as "machines"
-- We will follow a different, but equivalent, definition of monad than the standard one used in Haskell. The standard one is more practical and needs fewer functions and laws, but we believe that this one makes our analogy more natural.
-- The intuition behind is that monads represent machines that can perform computations differently from the standard way Haskell would. 

-- A machine (monad) is defined by 3 (polymorphic) functions:
class Monad m where
  -- Any haskell value, of any type, can be interpreted as a value written in the machine's "internal representation" of that type.
  return :: a ->  m a

  -- It is easy for the machine to "emulate itself": if we have a "machine code written in machine code," the machine can remove one of the layers and see it as simply "machine code."
  join :: m (m a) -> m a

  -- Any "vanilla" Haskell function can be translated into a function that deals with values written in "machine code."
  fmap :: (a -> b) -> m a -> m b

-- For an instance of this class be called a monad, there are laws that are expected to be valid. They are very natural with this machine metaphor. For example, one would expect that if we have a value `x` with type `m a` for some `a` (these are called *monadic values*), then `join (return x) = x` (in other words, that if we take something already in machine code and write it in machine code again, well, the machine can ignore this second layer). As another example, now for any value `x :: a` and any function `f :: a -> b` , one would expect that `fmap f (return x)` should be the same as `return (f x)` (performing a computation out of the machine, if possible, is compatible with performing it inside). We will postpone listing all the laws for another moment, to avoid losing our focus (see http://xenon.stanford.edu/~hwatheod/monads.html).

-- While an element of type `Int` is what Haskell calls an integer, an element of type `m Int` is what the machine calls an integer. Starting from the `2 :: Int`, we then have that `return 2 :: m Int` is a representation of the integer 2. But, and this is an important point, the machine can have elements of type `m Int` which are not achivable via `return`. One machine could call as an `m Int` something like "whatever integer is in this position of the memory when I read it," or "an integer that will be typed by the user," or "something that, if nothing goes wrong, will end up being an integer." We can say that the `return 2` means something like "the most basic representation the machine can have for the integer 2", or "the pure integer 2" or even "the constant integer 2." We will see different examples of monads shortly, so this will get clearer, but for now the goal is still to develop an intuition of what a monad is in general terms.

-- From what we just said, it is not surprising that, in general, one cannot read a what it is inside an `m a` and put it back in the vanilla type `a`. Once a value enters the machine, it will be in the machine forever (well, there are exceptions, but that is usually the case). In the examples we will show below, there is a `show` function with signature `show :: m a -> String`, so at least we can see what is going on, but not even this is necessary.

-- ## Monadic functions and bind

-- Most of the time, we communicate with a machine using functions that read "vanilla" values but produce values in "machine code", e.g., functions with signature `a -> m b` or `a -> b -> m c` in opposition to functions with signature `a -> b` or `a -> b -> c`. We will call these *monadic functions*. These are easy to write, since, again, we know how to put vanilla values into the machine but not the reverse.

-- (Formally speaking, we can define a *monadic function signature* to be any type `a -> x` where x is either of the form `m b` or it is, recursively, a monadic function signature.)

-- A very common situation is when we have a monadic value with type `m a`, for some `a` and want to pass it to a monadic function of type `a -> m b`. How could we do it? Well, we can't remove the `a` from inside the `m a`, but we can use the ability of the machine to "emulate itself!" The strategy is the following: we first use the `fmap` on our monadic function to get something with type `m a -> m (m b)`; we then it apply to our monadic value and get something with type `m (m b)`; finally, we resolve the redundancy of having an extra `m` by applying `join`. In other words, we use:

(>>=) :: (Monad m) => m a -> (a -> m b) -> m b
m_val >>= m_func = join (fmap m_func m_val)

-- This function is called *bind*. It is so powerful that we could have used it and `return` to define `join` and `fmap` instead of how we did. Actually, the default definition of the monad class in Haskell, which we avoided above, is using `return` and `(>>=)`! At this point, we could go back and remove the `{-# LANGUAGE NoImplicitPrelude #-}` and the `import Prelude hiding` but, still, we believe would make our machine metaphor more obscure. 

-- By design, bind doesn't follow the usual function application notation, `f x`, with the function on the left and the value on the right. And,indeed, there is a function `(=<<)` which follows this convention. But `(>>=)` is more common because it follows the order things usually happen: we have a value, then apply a function, get another value, pass it to another function, etc. This sequencing is something that agrees with the way many "machines" actually work.

-- Another strange thing is this name "bind". The intuition is that, although we can't read the internal value an `m a`, we can "bind its value" to the variable that we pass to the function `a -> m b`. This intuition will make more sense when we work with the *do notation*. But, we had enough general considerations. Let's put our hands on an example.

-- ## Waddler92's example: interpreting a simple lambda-calculus

-- To avoid confusing technicalities, we will assume we have a constant monad `M`, that we will choose which of the monads we will present below by commenting/uncommenting the obvious lines of code.

-- NEED: More explanation

-- ### Types defining our lambda-calculus:

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

varToValue :: Name -> Environment -> M Value
varToValue x []        = return Wrong
varToValue x ((y,b):e) = if x==y
                      then return b
                      else varToValue x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = tick >>= (\() -> return $ Num (i+j))
add _ _ = return Wrong

apply :: Value -> Value -> M Value
apply (Fun f) a = tick >>= (\() -> f a)
-- add (Num a) (Num b) = tick >>= (\() -> return (Num (i+j)))
apply _ _ = return Wrong

interp :: Term -> Environment -> M Value
interp (Con i) e = return (Num i)
interp (Var x) e = varToValue x e
interp (Add u v) e = interp u e >>=
                      (\a -> interp v e >>=
                        \b -> add a b)
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = interp t e >>= (\f -> interp u e >>= \a -> apply f a)

test :: Term -> String
test t = show (interp t [])

-- ## Examples of Monads

-- ### Machine 0: Identity

-- type M = Id

newtype Id a = Id a

instance Monad Id where
  return a = Id a
  join (Id (Id a)) = Id a
  fmap f (Id a) = Id (f a)

instance Show a => Show (Id a) where
  show (Id a) = show a

-- ### Machine 1: Error messages

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

-- Changes to the interpreter:

-- varToValue x [] = errorE ("Unbound variable: " ++ x)
-- add a b = errorE ("Should be numbers: " ++ show a ++ "," ++ show b)
-- apply f a = errorE ("Should be a function: " ++ show f)

-- ### Machine 2: Error messages with positions
-- COMMENTARIO: Parece uma composição de Reader com Error

-- Let `Position` be a type representing lines in a source file
newtype Position = Line Int

-- And then we add the positions to the Terms:
-- data Term = ... | At Position Term

-- We will assume that the parser, when reading the file, produces terms like `At p (App t (At q u))`, indicating that `p` is the position of the term `App t u` and that `q` is the position of the subterm u.

-- Machine we will define now communicates the Machine 1 (`E`) to produce error messages.

-- The idea is: for each position, the machine reads what is in the position and put it to run in the machine E
newtype P a = P (Position -> E a) 

instance Monad P where
  return :: a -> P a
  return a = P $ \position -> return a -- this last return is in the machine E

  join :: P (P a) -> P a
  join (P fstLayerRead) = P onlyLayerRead
    where 
      onlyLayerRead pos = case fstLayerRead pos of
        Success (P sndLayerRead) -> sndLayerRead pos
        Error msg                -> Error msg

  fmap :: (a -> b) -> P a -> P b
  fmap f (P reader) = P newReader
    where
      newReader pos = fmap f (reader pos) -- this fmap is from E

-- ### Machine 3: State

newtype StateMachine state a = SM (state -> (a, state))

type M = CountingMachine

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

-- instance for show only for the case the `state` is taken to be Int

type CountingMachine = StateMachine Int

instance Show a => Show (CountingMachine a) where
  show (SM machine) = let (a, s1) = machine 0 in
                      "Value: " ++ show a ++ "; " ++
                      "Count: " ++ show s1

-- The typing of `tick` makes clear that the value returned is not of interest. It is analogous to the use in an impure language of a function with result type (), indicating that the purpose of the function lies in a side efect.
tick :: CountingMachine ()
tick = SM $ \s -> ((), s+1)

-- Modifications:
-- apply (Fun f) a = tick >>= (\() -> f a)
-- add (Num a) (Num b) = tick >>= (\() -> return (Num (i+j)))



-- If we want to read the current state, we can use:
fetch :: StateMachine state state
fetch = SM machine
  where
    machine s = (s, s)

-- ### Machine 4: Output (Writer?)
-- ### Machine 5: Non-deterministic choice (List)
-- ### Machine 6: Backwards state (exoteric one, needs lazyness)

term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 10)) 