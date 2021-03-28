{-# LANGUAGE NoImplicitPrelude, GADTs , DataKinds, TypeFamilies, TypeOperators, RankNTypes, DeriveFunctor #-}

module Singletons where

import Prelude hiding (drop, take, head, tail, index, zipWith, replicate, map, (++))

data Vec a n where
  VNil :: Vec a Zero
  VCons :: a -> Vec a n -> Vec a (Succ n)

-- promoted to type level by data kinds
data Nat = Zero | Succ Nat

data SNat a where
  SZero :: SNat Zero
  SSucc :: SNat a -> SNat (Succ a)

type family (a :: Nat) :< (b :: Nat) :: Bool
type instance m :< Zero = False
type instance Zero :< Succ n = True
type instance (Succ m) :< (Succ n) = m :< n

type family (Add (a :: Nat) (b :: Nat)) :: Nat
type instance (Add Zero n) = n
type instance (Add (Succ n) m) = Succ (Add n m)

type family (Pred (a :: Nat)) :: Nat
type instance (Pred (Succ n)) = n

type family (Sub (a :: Nat) (b :: Nat)) :: Nat
type instance (Sub a Zero) = a
type instance (Sub Zero a) = Zero
type instance (Sub (Succ a) (Succ b)) = (Sub a b)

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance (Min Zero b) = Zero
type instance (Min a Zero) = Zero
type instance (Min (Succ a) (Succ b)) = Succ (Min a b)

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons x xs) = x
index (SSucc m) (VCons _ xs) = index m xs

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate s (SSucc n) = VCons s (replicate s n)

-- Both vectors must be of equal length
zipWith :: (v -> w -> a) -> Vec v n -> Vec w n -> Vec a n
zipWith _ VNil VNil = VNil
zipWith f (VCons v vs) (VCons w ws) = VCons (f v w) $ zipWith f vs ws

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ b = b
(VCons a VNil) ++ b = VCons a b
(VCons a as) ++ b = VCons a (as ++ b)

-- The semantics should match that of take for normal lists.
take :: SNat m -> Vec v n -> Vec v (Min m n)
take SZero _ = VNil
take _ VNil = VNil
take (SSucc n) (VCons x xs) = VCons x $ take n xs

-- The semantics should match that of drop for normal lists.
drop :: SNat m -> Vec v n -> Vec v (Sub n m)
drop SZero v = v
drop (SSucc n) VNil = VNil
drop (SSucc n) (VCons _ xs) = drop n xs

head :: ((Zero :< n) ~ True) => Vec v n -> v
head (VCons x _) = x

tail :: ((Zero :< m) ~ True) => Vec v m -> Vec v (Pred m)
tail (VCons _ xs) = xs