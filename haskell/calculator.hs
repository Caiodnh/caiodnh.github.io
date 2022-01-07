{-# LANGUAGE InstanceSigs #-} -- To be able to rewrite the signatures when defining instances
{-# LANGUAGE UndecidableInstances #-} -- To be able to automatically deduce `Monad` from `Machine`
{-# LANGUAGE FlexibleInstances #-} -- To avoid creating newtypes to define some intances

module Machines where

-- # Machine class

class Machine m where
  returnM :: a ->  m a
  joinM :: m (m a) -> m a
  fmapM :: (a -> b) -> m a -> m b
  bindM :: m a -> (a -> m b) -> m b
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

-- # Expressions

-- ## Choices for machine M

-- type M = Id
-- type M = E
type M = Reader Double
-- type M = StateMachine Int
-- type M = []

-- ## Types

data Expr = Num Double
          | Add Expr Expr
          | Mult Expr Expr
          | Pi
          -- | Div Expr Expr deriving Show
-- Add for E
          -- | Div Expr Expr
          -- | Sqrt Expr
-- Add for Reader
          -- | Pi
-- Add for State:
          -- | Count

-- ## Functions

eval :: Expr -> M Double
eval (Num d)    = return d
eval (Add a b)  = eval a >>=
                    (\x -> eval b >>=
                      (\y -> return (x + y)))
eval (Mult a b) = eval a >>=
                    (\x -> eval b >>=
                      (\y -> return (x * y)))
eval Pi         = R $ \pi -> pi
-- eval (Div a b) = eval b >>=
--                    (\y -> if y == 0
--                             then Error $ "Zero division error in Div (" ++ show a ++ ") (Num " ++ show y ++ ")"
--                             else eval a >>= 
--                               (\x -> return $ x/y))
-- eval (Both a b) = eval a >>= \x -> eval b >>= \y -> [x, y]
-- eval Count = SM $ \s -> (fromIntegral s, s+10)
-- eval Pi = R $ \pi -> pi
-- eval (Div a b) = eval b >>= \y ->
--                    if y == 0 then Error "We can't divide by zero."
--                    else 
--                      eval a >>= \x -> return $ x/y
-- # Machines

-- ## Machine 0

newtype Id a = Id a

instance Machine Id where
  returnM a = Id a
  joinM (Id (Id a)) = Id a
  fmapM f (Id a) = Id (f a)
  bindM :: Id a -> (a -> Id b) -> Id b
  bindM (Id a) (f) = f a

instance Show a => Show (Id a) where
  show (Id a) = show a

-- ## Reader

newtype Reader value a = R (value -> a)

withPiEqual :: Reader value a -> value -> a
withPiEqual (R f) pi = f pi

instance Machine (Reader value) where
  returnM a = R $ \pi -> a

  -- think about Add (Mult (Num 2) Pi) Pi
  joinM :: Reader value (Reader value a) -> Reader value a
  -- metaFunc :: value -> (Reader value a)
  joinM metaCode = R newFunc
    where
      -- newFunc :: value -> a
      newFunc pi = (metaCode `withPiEqual` pi) `withPiEqual` pi

  fmapM f (R code) = R newCode
    where
      newCode pi = f (code pi)

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

  bindM :: E a -> (a -> E b) -> E b
  bindM (Success a) f = f a
  bindM (Error msg) f = Error msg

instance Show a => Show (E a) where
  show (Success a) = show a
  show (Error msg) = "ERROR! " ++ msg

-- ## Machine 2

newtype StateMachine state a = SM {runMachine :: state -> (a, state)}

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

bind :: StateMachine Int a -> (a -> StateMachine Int b) -> StateMachine Int b
bind m_a f = SM newMachine
  where
    newMachine s0 = let (a, s1) = runMachine m_a s0 in runMachine (f a) s1

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