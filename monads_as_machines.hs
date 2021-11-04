{-# LANGUAGE InstanceSigs #-} -- To be able to rewrite the signatures when defining instances
{-# LANGUAGE UndecidableInstances #-} -- To be able to automatically deduce `Monad` from `Machine`
{-# LANGUAGE FlexibleInstances #-} -- To avoid creating newtypes to define some intances

module Machine where

-- # Machine class

class Machine m where
  return_ :: a ->  m a
  join_ :: m (m a) -> m a
  fmap_ :: (a -> b) -> m a -> m b

bind_ :: (Machine m) => m a -> (a -> m b) -> m b
bind_ m_val m_func = join_ (fmap_ m_func m_val)

ap_ :: (Machine m) => m (a -> b) -> m a -> m b
ap_ m_f m_x = m_f `bind_` (\f -> m_x `bind_` (\x -> return_ (f x)))

-- # Back to Monads

instance Machine m => Functor m where
  fmap = fmap_

instance Machine m => Applicative m where
  pure = return_
  (<*>) = ap_

instance Machine m => Monad m where
  return = return_
  (>>=) = bind_
-- # Lambda Calculus

-- ## Choices for machine M

-- type M = Id
-- type M = E
type M = StateMachine Int

-- ## Types

type Name = String

data Term = Con Int
          | Var Name
          | Add Term Term
          | Lam Name Term
          | App Term Term

data Value = Wrong 
           | Num Int
           | Fun (Value -> M Value)

type Environment = [(Name, Value)]

-- Examples

-- term0 is (\x -> x + x) (10 + 11)
term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 11))

-- ## Instances
instance Show Value where
  show Wrong   = "<wrong>"
  show (Num i) = show i
  show (Fun f) = "<function>"

-- ## Functions

varToValue :: Name -> Environment -> M Value
varToValue x []        = return Wrong
-- Change for Machine 1:
-- varToValue x [] = errorE ("Unbound variable: " ++ x)
varToValue x ((y,b):e) = if x==y
                      then return b
                      else varToValue x e

add :: Value -> Value -> M Value
-- add (Num i) (Num j) = return $ Num (i+j)
-- Change for Machine 2:
add (Num i) (Num j) = tick >>= (\() -> return (Num (i+j)))
add _ _ = return Wrong
-- ### Change for Machine 1
-- add _ _ = errorE ("Should be numbers: " ++ show a ++ "," ++ show b)

apply :: Value -> Value -> M Value
-- apply (Fun f) a = f a
-- Change for Machine 2:
apply (Fun f) a = tick >>= (\() -> f a)
apply _ _ = return Wrong
-- Changes for Machine 1:
-- apply _ _ = errorE ("Should be a function: " ++ show f)

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

-- # Machines

-- ## Machine 0

newtype Id a = Id a

instance Machine Id where
  return_ a = Id a
  join_ (Id (Id a)) = Id a
  fmap_ f (Id a) = Id (f a)

instance Show a => Show (Id a) where
  show (Id a) = show a

-- ## Machine 1

data E a = Error String | Success a 

instance Machine E where
  return_ :: a -> E a
  return_ a = Success a

  join_ :: E (E a) -> E a
  join_ (Success (Success a)) = Success a
  join_ (Success (Error msg)) = Error msg
  join_ (Error msg)           = Error msg

  fmap_ :: (a -> b) -> E a -> E b
  fmap_ f (Success a) = Success (f a)
  fmap_ f (Error msg) = Error msg

instance Show a => Show (E a) where
  show (Success a) = show a
  show (Error msg) = "ERROR! " ++ msg

-- ## Machine 2

newtype StateMachine state a = SM (state -> (a, state))

instance Machine (StateMachine state) where
  return_ :: a -> StateMachine state a
  return_ a = SM $ \s -> (a, s)
  
  join_ :: StateMachine state (StateMachine state a) -> StateMachine state a
  join_ (SM metaMachine) = SM newMachine
    where
      newMachine s0 = let (SM machine, s1) = metaMachine s0 in machine s1

  fmap_ :: (a -> b) -> StateMachine state a -> StateMachine state b
  fmap_ f (SM machine) = SM newMachine
    where
      newMachine s0 = let (a, s1) = machine s0 in (f a, s1)

fetch :: StateMachine state state
fetch = SM machine
  where
    machine s = (s, s)

instance Show a => Show (StateMachine Int a) where
  show (SM machine) = let (a, s1) = machine 0 in
                      "Value: " ++ show a ++ "; " ++
                      "Count: " ++ show s1

tick :: StateMachine Int ()
tick = SM $ \s -> ((), s+1)
