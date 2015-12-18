{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE ConstraintKinds       #-}

module Category
       ( type (~>)
       , Category (..)
       , Functor (..)
       , Endofunctor
       ) where

------------------------------------------------------------
-- Category
------------------------------------------------------------

-- |The ~> type operator denotes a morphism from object a to object b in category c
type (a ~> b) c = c a b

-- |Category defined by morphisms
class Category (c :: k -> k -> *) where
  -- |The identity functor that maps all objects/morphisms back to themselves
  id :: (a ~> a) c
  -- |The composition rule for combining morphisms associatively
  (.) :: (y ~> z) c -> (x ~> y) c -> (x ~> z) c

------------------------------------------------------------
-- Functor
------------------------------------------------------------

-- |A functor f maps between objects and morphisms from c1 to c2 that preserved
-- |morphism composition and identities
class (Category c1, Category c2) => Functor f c1 c2 where
  -- Functor laws:
  --   Law 1: fmap id = id
  --   Law 2: fmap (a . b) = (fmap a) . (fmap b)
  fmap :: c1 a b -> c2 (f a) (f b)

-- The composition of two functors is itself a functor as well

type (g :.: f) x = g (f x)

-- ERROR!
instance (Functor f c1 c2, Functor g c2 c3) => Functor (g :.: f) c1 c3 where

------------------------------------------------------------
-- Endofunctor
------------------------------------------------------------

type Endofunctor f c = Functor f c c
