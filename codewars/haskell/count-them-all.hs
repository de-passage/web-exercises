{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications #-}
module Counting where
import Counting.Preloaded
import Data.Proxy
import Data.Void
import Data.Functor.Const
import Data.Functor.Identity
import Data.Functor.Sum
import Data.Functor.Product
import Data.Functor.Compose
import Data.List
import Data.Maybe
import Control.Monad

{- in Preloaded:
data Nat = Z | S Nat deriving (Show, Eq, Ord)
instance Num Nat -- so (2 :: Nat) == S (S Z)
instance Enum Nat -- so ([0..3] :: [Nat]) == [Z, S Z, S (S Z)]
instance Real Nat
instance Integral Nat -- so (2 ^ 3 :: Nat) == S (S (S (S (S (S (S (S Z)))))))
-}

newtype Count x = Count { getCount :: Nat } deriving (Show, Eq, Ord)

-- | helper functions
mapC :: (Nat -> Nat) -> Count a -> Count b
mapC f (Count a) = Count (f a)

liftC2 :: (Nat -> Nat -> Nat) -> Count a -> Count b -> Count c
liftC2 f (Count a) (Count b) = Count (f a b)

coerceC :: Count a -> Count b
coerceC (Count a) = Count a

-- | Countable
class Countable c where
  count :: Count c
  -- if you are using `Proxy` implement `count` from `count'` and vice versa
  -- count' :: Proxy c -> Count c
  -- count' = error "from count"
  
infinity :: Nat
infinity = S infinity

instance Countable Void where count = Count (0 :: Nat)
instance Countable () where count = Count (1 :: Nat)
instance Countable Bool where count = Count (2 :: Nat)
instance Countable Nat where count = Count infinity

-- | Factor
class Factor f where
  factor :: Count c -> Count (f c)
  -- factor' :: Proxy f -> Count c -> Count (f c) -- optional

instance (Factor f, Countable c) => Countable (f c) where
  count = factor (count @c)

instance Factor Maybe where factor = mapC succ
instance Factor Identity where factor = coerceC
instance Factor Proxy where factor = const (Count (1 :: Nat))
instance Factor Count where factor = const $ coerceC (count @Nat)
instance Factor [] where 
  factor (Count Z) = (Count (1 :: Nat)) -- Even [Void] has one element: []
  factor (Count n) = Count infinity
instance Countable c => Factor (Const c) where factor = const $ coerceC $ count @c
instance Countable c => Factor (Either c) where factor = liftC2 (+) (count @c)
instance Countable c => Factor ((,) c) where factor = liftC2 (*) (count @c)
instance Countable c => Factor ((->) c) where factor c = liftC2 (^) c (count @c)
instance (Factor f, Factor g) => Factor (Sum f g) where factor c = liftC2 (+) (factor @f c) (factor @g c) 
instance (Factor f, Factor g) => Factor (Product f g) where factor c = liftC2 (*) (factor @f c) (factor @g c)
instance (Factor f, Factor g) => Factor (Compose f g) where factor c = coerceC $ factor @f (factor @g c)

-- | Listable
class Countable a => Listable a where
  list :: [a]
  -- list' :: Proxy a -> [a] -- optional
-- Data.List.genericLength (list :: [a]) `shouldBe` getCount (count :: Count a)

instance Listable Void where list = []
instance Listable () where list = [()]
instance Listable Bool where list = [True, False]
instance Listable Nat where 
  list = list' Z
    where list' nat = nat : list' (S nat)

instance Listable c => Listable (Maybe c) where list = Nothing : map Just (list @ c)
instance Listable c => Listable [c] where list = inits (cycle (list @c))
instance (Listable a, Listable b) => Listable (Either a b) where list = map Left (list @a) ++ map Right (list @b)
instance (Listable a, Listable b) => Listable (a, b) where list = liftM2 (,) (list @a) (list @b)
instance (Eq a, Listable a, Listable b) => Listable (a -> b) where 
  -- A function can be thought of as a map of every possible input to an output. The list of all functions is the list of all possible maps,
  -- i.e. the cartesian product of n vectors of possible outputs (list b), n beeing the cardinality of a
  list = let 
    nA = fromEnum $ getCount $ count @a
    allA = list @a
    allB = list @b
    -- sequence computes the cartesian product of the lists produced by replicate, then we take each of these lists, associate it with a list of input (allA) and create a map out of it
    allMaps = zipWith (zipWith (,)) (repeat allA) $ sequence $ replicate nA allB
    -- the functions are then litteraly a lookup of the output from the input
    f lb a = fromJust $ lookup a lb
    in
      map f allMaps