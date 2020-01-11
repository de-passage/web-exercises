{-# LANGUAGE 
  FlexibleInstances, 
  UndecidableInstances, 
  InstanceSigs,
  ScopedTypeVariables,
  RankNTypes #-}

module PC where

import Data.List
import Data.Void

type ISO a b = (a -> b, b -> a)
-- See https://www.codewars.com/kata/isomorphism

symm :: ISO a b -> ISO b a
symm (ab, ba) = (ba, ab)

substL :: ISO a b -> (a -> b)
substL = fst

substR :: ISO a b -> (b -> a)
substR = snd


liftISO2 :: ISO a b -> ISO (a -> a -> a) (b -> b -> b)
liftISO2 (ab, ba) = (left, right)
  where 
    left aaa b1 b2 =  ab $ aaa (ba b1) (ba b2)
    right bbb a1 a2 = ba $ bbb (ab a1) (ab a2)

-- A Natural Number is either Zero,
-- or a Successor (1 +) of Natural Number.
-- We have (+)/(*) on Natural Number, or (-) it.
-- Since Natrual Number do not have negative, forall x, 0 - x = 0.
-- We also have pow on Natrual Number
-- Since Haskell is lazy, we also have infinity

class Nat n where
  zero :: n
  successor :: n -> n
  nat :: a -> (n -> a) -> n -> a -- Pattern Matching
  iter :: a -> (a -> a) -> n -> a -- Induction
  plus, minus, mult, pow :: n -> n -> n
  inf :: n
  inf = successor inf
  divide :: n -> n -> n
  l `divide` r | l == 0 && r == 0 = undefined
  l `divide` r | l < r = 0
  l `divide` r | otherwise = successor $ (l `minus` r) `divide` r
  -- notice (l `divide` 0) when l is not 0 will return inf
  isoP :: ISO n Peano -- See below for the definition of Peano
  isoP = (iter zero successor, iter zero successor)
  toP :: n -> Peano
  toP = substL isoP

instance {-# OVERLAPPABLE #-} Nat n => Show n where
  show = show . toP

instance {-# OVERLAPPABLE #-} Nat n => Eq n where
  l == r = toP l == toP r

instance {-# OVERLAPPABLE #-} Nat n => Ord n where
  l `compare` r = toP l `compare` toP r

instance {-# OVERLAPPABLE #-} Nat n => Num n where
  abs = id
  signum = nat zero (const 1)
  fromInteger 0 = zero
  fromInteger n | n > 0 = successor $ fromInteger (n - 1)
  (+) = plus
  (*) = mult
  (-) = minus

-- We can encode Natrual Number directly as Algebraic Data Type(ADT).
data Peano = O | S Peano deriving (Show, Eq, Ord)

-- Remember, 0 - x = 0 for all x.
instance Nat Peano where
  zero :: Peano
  zero = O
  
  successor :: Peano -> Peano
  successor n = S n
  
  nat :: a -> (Peano -> a) -> Peano -> a
  nat a _ O = a
  nat _ f (S n) = f n
  
  iter :: a -> (a -> a) -> Peano -> a
  iter a f O = a
  iter a f (S n) = iter (f a) f n
  
  plus :: Peano -> Peano -> Peano
  plus  = flip iter successor
  
  minus :: Peano -> Peano -> Peano
  minus = flip iter (\p ->
                        case p of
                          O -> O
                          S n -> n)

  mult :: Peano -> Peano -> Peano
  mult p1 p2 = iter 0 (plus p1) p2
  
  pow :: Peano -> Peano -> Peano
  pow p1 p2 = iter 1 (mult p1) p2

-- Peano is very similar to a basic data type in Haskell - List!
-- O is like [], and S is like :: (except it lack the head part)
-- When we want to store no information, we can use (), a empty tuple
-- This is different from storing nothing (called Void in Haskell),
-- as we can create a value of () by using (), 
-- but we cannot create a value of Void.

-- Notice how you can implement everything once you have isoP,
-- By converting to Peano and using Nat Peano?
-- Dont do that. You wont learn anything.
-- Try to use operation specific to list.
instance Nat [()] where
  zero :: [()]
  zero = []
  
  successor :: [()] -> [()]
  successor = (():)
  
  nat :: a -> ([()] -> a) -> [()] -> a
  nat a _ [] = a
  nat _ f (_:l) = f l
  
  iter :: a -> (a -> a) -> [()] -> a
  iter a f = foldr (\_ acc -> f acc) a
  
  plus :: [()] -> [()] -> [()]
  plus = (++)
  
  minus :: [()] -> [()] -> [()]
  minus = (.length).(flip drop)
  
  mult :: [()] -> [()] -> [()]
  mult = (>>)

  pow :: [()] -> [()] -> [()]
  pow = iter [()] . mult
  
-- Instead of defining Nat from zero, sucessor (and get Peano),
-- We can define it from Pattern Matching
newtype Scott = Scott { runScott :: forall a. a -> (Scott -> a) -> a }
instance Nat Scott where
  zero :: Scott
  zero = Scott const
  
  successor :: Scott -> Scott
  successor s = Scott (\a f' -> f' s)
  
  nat :: a -> (Scott -> a) -> Scott -> a
  nat a f s = runScott s a f
  
  iter :: a -> (a -> a) -> Scott -> a
  iter a f s = runScott s a (iter (f a) f)
  
  -- Other operation on Scott numeral is sort of boring,
  -- So we implement it using operation on Peano.
  -- You shouldnt do this - I had handled all the boring case for you.
  plus = substR (liftISO2 isoP) plus
  minus = substR (liftISO2 isoP) minus
  mult = substR (liftISO2 isoP) mult
  pow = substR (liftISO2 isoP) pow

-- Or from induction!
newtype Church = Church { runChurch :: forall a. (a -> a) -> a -> a }
instance Nat Church where
  zero :: Church
  zero = Church (flip const)
  
  successor :: Church -> Church
  successor (Church f) = Church (\f' a -> f' $ f f' a)
  
  nat :: a -> (Church -> a) -> Church -> a
  nat a f c = runChurch c (const f) (const a) $ (predecessor c)
  
  iter :: a -> (a -> a) -> Church -> a
  iter a f c = runChurch c f a
  
  plus :: Church -> Church -> Church
  plus (Church a) (Church b) = Church (\f c -> a f (b f c))
  
  minus :: Church -> Church -> Church
  minus a b = runChurch b predecessor a

  mult :: Church -> Church -> Church
  mult (Church a) (Church b) = Church (\f c -> a (b f) c)
  
  pow :: Church -> Church -> Church
  pow (Church a) (Church b) = Church (b a)
  
predecessor :: Church -> Church
predecessor c = snd $ iter (zero, zero) (\(a, _) -> (successor a, a)) c
  
  -- Try to implement the calculation (except minus) in the primitive way.
  -- Implement them by constructing Church explicitly.
  -- So plus should not use successor,
  -- mult should not use plus,
  -- exp should not use mult.