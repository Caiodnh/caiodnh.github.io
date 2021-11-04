{-# LANGUAGE InstanceSigs #-} -- To be able to rewrite the signatures when defining instances
{-# LANGUAGE UndecidableInstances #-} -- To be able to automatically deduce `Monad` from `Machine`
{-# LANGUAGE FlexibleInstances #-} -- To avoid creating newtypes to define some intances

module Machines where

-- # Machine class

class Machine m where
  returnM :: a ->  m a
  joinM :: m (m a) -> m a
  fmapM :: (a -> b) -> m a -> m b

bindM :: (Machine m) => m a -> (a -> m b) -> m b
bindM m_val m_func = joinM (fmapM m_func m_val)

apM :: (Machine m) => m (a -> b) -> m a -> m b
apM m_f m_x = m_f `bindM` (\f -> m_x `bindM` (\x -> returnM (f x)))

-- # Back to Monads

instance Machine m => Functor m where
  fmap = fmapM

instance Machine m => Applicative m where
  pure = returnM
  (<*>) = apM

instance Machine m => Monad m where
  return = returnM
  (>>=) = bindM

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
-- Add for variation of Machine 2:
        --  | Count

data Value = Wrong 
           | Num Int
           | Fun (Value -> M Value)

type Environment = [(Name, Value)]

-- ## Examples of terms

-- term0 is (\x -> x + x) (10 + 11)
term0 :: Term
term0 = App (Lam "x" (Add (Var "x") (Var "x"))) (Add (Con 10) (Con 11))

-- term1
term1 :: Term
term1 = App (Con 1) (Con 2)

-- term2 (for Machine 2)
-- term2 :: Term
-- term2 = Add (Add (Con 1) (Con 2)) Count

-- term3 (for Machine 3)
-- term3 :: Term
-- term3 = Add (Out (Con 41)) (Out (Con 1))

-- term4 (for Machine 4)
-- App (Lam "x" (Add (Var "x") (Var "x"))) (Amb (Con 1) (Con 2))

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
-- Add for variation of Machine 2:
-- interp Count e = fetch >>= (\i -> return (Num i))


test :: Term -> String
test t = show (interp t [])

-- # Machines

-- ## Machine 0

newtype Id a = Id a

instance Machine Id where
  returnM a = Id a
  joinM (Id (Id a)) = Id a
  fmapM f (Id a) = Id (f a)

instance Show a => Show (Id a) where
  show (Id a) = show a

-- ## Machine 1

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

-- ## Machine 2

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

instance Show a => Show (StateMachine Int a) where
  show (SM machine) = let (a, s1) = machine 0 in
                      "Value: " ++ show a ++ "; " ++
                      "Count: " ++ show s1

tick :: StateMachine Int ()
tick = SM $ \s -> ((), s+1)

-- Reading the State:

fetch :: StateMachine state state
fetch = SM machine
  where
    machine s = (s, s)


