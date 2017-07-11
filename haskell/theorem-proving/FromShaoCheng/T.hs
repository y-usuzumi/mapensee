{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}

module Data.HList.Simple.TypeUtils where

import Data.Kind
import Data.Proxy
import Data.Type.Equality

type family xs :++ ys where
    '[] :++ ys = ys
    (x ': xs) :++ ys = x ': (xs :++ ys)
infixr 5 :++

type AppendNil xs = xs :~: (xs :++ '[])

appendNilBase :: AppendNil '[]
appendNilBase = Refl

appendNilInd :: AppendNil xs -> AppendNil (x ': xs)
appendNilInd Refl = Refl

appendNil :: AppendNil '[] -> (AppendNil xs -> AppendNil (x ': xs)) -> AppendNil xs
appendNil appendNilBase appendNilInd = Refl
