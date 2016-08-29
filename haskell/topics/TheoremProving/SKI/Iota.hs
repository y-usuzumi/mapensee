{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}

module Iota where

data T t where
  U :: T (a -> b)
  (:$) :: T (a -> b) -> T a -> T b

i = U :$ U
k = U :$ (U :$ (U :$ U))
s = U :$ (U :$ (U :$ (U :$ U)))

instance Show (T t) where
  show U = "u"
  show (a :$ b) = show a ++ "(" ++ show b ++ ")"

type TChurch a = T ((a -> a) -> (a -> a))

encodeCharT :: Char -> TChurch a
encodeCharT '\n' = k :$ i
encodeCharT c = let
  p = encodeCharT (pred c)
  in
  s
  :$ ( s
       :$ (k :$ s)
       :$ (s :$ (k :$ k) :$ i)
     )
  :$ ( s
       :$ (s :$ (k :$ s) :$ (s :$ (k :$ k) :$ (s :$ (k :$ p) :$ i)))
       :$ (k :$ i)
     )

