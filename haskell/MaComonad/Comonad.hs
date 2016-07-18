module Comonad ( Comonad (..)
               ) where

class Functor w => Comonad w where
  (=>>) :: w a -> (w a -> b) -> w b
  coreturn :: w a -> a
  cojoin :: w a -> w (w a)
  x =>> f = fmap f (cojoin x)
