{- `s` stands for `suspension functor` -}

import           Control.Monad.Trans

newtype Coroutine s m r = Coroutine { resume :: m (CoroutineState s m r) }
data CoroutineState s m r = Run (s (Coroutine s m r))
                          | Done r

instance (Functor s, Monad m) => Functor (Coroutine s m) where
  fmap f coroutine = Coroutine $ do
    cs <- resume coroutine
    case cs of
      Run s' -> return (Run (fmap (fmap f) s'))
      Done r -> return (Done (f r))

instance (Functor s, Monad m) => Applicative (Coroutine s m) where
  pure = Coroutine . pure . Done
  fcoroutine <*> coroutine = Coroutine $ do
    cs <- resume fcoroutine
    case cs of
      Run s  -> return $ Run (fmap (<*> coroutine) s)
      Done r -> resume (fmap r coroutine)

instance (Functor s, Monad m) => Monad (Coroutine s m) where
  return = pure
  coroutine >>= f = Coroutine $ do
    cs <- resume coroutine
    case cs of
      Run s  -> return $ Run (fmap (\c -> c >>= f) s)
      Done r -> resume (f r)
