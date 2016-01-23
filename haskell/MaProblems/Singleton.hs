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
type instance (Add (Succ m) n) = Succ (Add m n)

type family (Sub (a :: Nat) (b :: Nat)) :: Nat
type instance (Sub a Zero) = a
type instance (Sub Zero a) = Zero
type instance (Sub (Succ a) (Succ b)) = Sub a b

type family (Min (a :: Nat) (b :: Nat)) :: Nat
type instance (Min Zero m) = Zero
type instance (Min m Zero) = Zero
type instance (Min (Succ m) (Succ n)) = Succ (Min m n)

map :: (a -> b) -> Vec a n -> Vec b n
map f VNil = VNil
map f (VCons x xs) = VCons (f x) (map f xs)

index :: ((a :< b) ~ True) => SNat a -> Vec s b -> s
index SZero (VCons elem _) = elem
index (SSucc a) (VCons elem elems) = index a elems

replicate :: s -> SNat a -> Vec s a
replicate _ SZero = VNil
replicate elem (SSucc snat) = VCons elem (replicate elem snat)

-- Both vectors must be of equal length
zipWith :: (a -> b -> c) -> Vec a m -> Vec b n -> Vec c (Min m n)
zipWith _ _ VNil = VNil
zipWith _ VNil _ = VNil
zipWith f (VCons a1 as1) (VCons a2 as2) = VCons (f a1 a2) (zipWith f as1 as2)

(++) :: Vec v m -> Vec v n -> Vec v (Add m n)
VNil ++ b = b
(VCons a as) ++ b = VCons a (as ++ b)

-- The semantics should match that of take for normal lists.
take :: SNat a -> Vec s b -> Vec s (Min a b)
take SZero _ = VNil
take _ VNil = VNil
take (SSucc a) (VCons elem elems) = VCons elem (take a elems)

-- The semantics should match that of drop for normal lists.
drop :: SNat a -> Vec s b -> Vec s (Sub b a)
drop _ VNil = VNil
drop SZero v = v
drop (SSucc a) (VCons elem elems) = drop a elems

head :: (Zero :< a) ~ True => Vec s a -> s
head (VCons elem _) = elem

tail :: (Zero :< a) ~ True => Vec s (Succ a) -> Vec s a
tail (VCons _ elems) = elems





