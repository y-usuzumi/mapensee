module U ( U (..)
         , left
         , right
         ) where

import Comonad

data U x = U [x] x [x] deriving Show

left :: U x -> U x
left (U (a:as) b c) = U as a (b:c)
left (U [] _ _) = error "No more left"

right :: U x -> U x
right (U a b (c:cs)) = U (b:a) c cs
right (U _ _ []) = error "No more right"

instance Functor U where
  fmap f (U a b c) = U (fmap f a) (f b) (fmap f c)

instance Comonad U where
  cojoin a = U (tail $ iterate left a) a (tail $ iterate right a)
  coreturn (U _ b _) = b
