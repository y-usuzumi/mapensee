{-# LANGUAGE GADTs              #-}
{-# LANGUAGE KindSignatures     #-}

module Naive1 where

import Control.Monad.State hiding (runState)

-- 感谢千人在https://www.zhihu.com/question/30642659上对free monad深入浅出的解释。

-- 首先，free monad的意义在于分离monadic DSL和对这个DSL的解释。

-- 比如，我们的DSL里有say和ask两个原语：

say :: String -> Interaction ()
say = Say

ask :: Interaction String
ask = Ask

naive1 :: Interaction ()
naive1 = do
  say "hello"
  say "who are you ?"
  name <- ask
  say ("nice to meet you, " ++ name ++ "!")

data Interaction :: * -> * where
  Say :: String -> Interaction ()
  Ask :: Interaction String
  Return :: a -> Interaction a
  Bind :: Interaction a -> (a -> Interaction b) -> Interaction b

instance Functor Interaction where
  fmap f a = Bind a (return . f)

instance Applicative Interaction where
  pure = Return
  a <*> b = Bind a (<$> b)

instance Monad Interaction where
  return = Return
  (>>=) = Bind

-- 我们按照IO对其进行解释：

runIO :: Interaction a -> IO a
runIO (Say a) = putStrLn a
runIO Ask = getLine
runIO (Return a) = return a
runIO (Bind a f) = runIO a >>= runIO . f

-- 我们还可以按照State对其进行解释

runState :: Interaction a -> State String a
runState (Say a) = put a
runState Ask = get
runState (Return a) = return a
runState (Bind a f) = runState a >>= runState . f
