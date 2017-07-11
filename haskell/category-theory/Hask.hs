{-# LANGUAGE TypeSynonymInstances #-}

module Hask where

import Category

type Hask = (->)

instance Category Hask where
  id x = x
  (f . g) x = f (g x)
