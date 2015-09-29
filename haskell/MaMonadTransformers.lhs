(https://en.wikibooks.org/wiki/Haskell/Monad_transformers)

We have seen how monads can help handling IO actions, Maybe, lists,and state.
With monads providing a common way to use such useful general-purpose tools,
a natural thing we might want to do is using the capabilities of several
monads at once. For instance, a function could use both I/O and Maybe
exception handling. While a type like IO (Maybe a) would work just fine,
it would force us to do pattern matching within IO do-blocks to extract
values, something that the Maybe monad was meant to spare us from.

Enter monad transformers: special types that allow us to roll two monads into
a single one that shares the behavior of both.

> module Main where
>
> import Test.Hspec
> import Data.Char
> import Data.IORef
> import Control.Monad
> import Control.Monad.Trans
> import Control.Monad.Trans.Maybe
> import Prelude hiding (getLine)
> import System.IO hiding (getLine)
> import System.IO.Unsafe

Definition of MaybeT:
< newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }
<
< instance Monad m => Monad (MaybeT m) where
<   return = MaybeT . return . Just
<   (>>=) :: MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
<   x >>= f = MaybeT $ do maybe_value <- runMaybeT x
<                         case maybe_value of
<                           Nothing -> return Nothing
<                           Just value -> runMaybeT $ f value

Other useful stuff:
< instance Monad m => MonadPlus (MaybeT m) where
<   mzero = MaybeT $ return Nothing
<   mplus x y = MaybeT $ do maybe_value <- runMaybeT x
<                          case maybe_value of
<                            Nothing -> runMaybeT y
<                            Just _ -> return maybe_value

< instance MonadTrans MaybeT where
<   lift = MaybeT . (liftM Just)

NOTE: All following code is for demonstration purpose only.
      Do NOT use it in production code!!!

> isValid :: String -> Bool
> isValid s =
>   length s >= 8
>   && any isAlpha s
>   && any isNumber s
>   && any isPunctuation s
>
> {-# NOINLINE getLineRef #-}
> getLineRef :: IORef String
> getLineRef = unsafePerformIO $ newIORef ""
>
> getLine :: IO String
> getLine = readIORef getLineRef
>
> getValidPassphrase :: MaybeT IO String
> getValidPassphrase = do
>   s <- lift getLine
>   guard (isValid s)
>   return s
>
> askPassphrase :: MaybeT IO ()
> askPassphrase = do
>   lift $ putStrLn "Insert your new passphrase:"
>   value <- getValidPassphrase
>   lift $ putStrLn "Storing in database..."
>

Now on to the StateT transformer:

Recall the definition of State monad:
< newtype State s a = State { runState :: (s -> (a, s))}
<
< instance Monad (State s) where
<   return a = State $ \s -> (a, s)  -- Running with any state will return `a`
<   (State x) >>= f = State $ \s ->
<     let (val, s') = x s  -- Run the previous statefunc and get a val and new state
<     in runState (f val) s'  -- Passing `val` to f will make you a new-new statefunc
<                             -- Running the new-new statefunc with the new state
<                             -- will bring you (newVal, newNewState)

(
  Sidenote: Actually, it's implemented just the opposite way:
  < type State s = StateT a Identity
  But for simplicity we'll pretend the above.
)

Here comes StateT:
< newtype StateT s m a = StateT { runStateT :: (s -> m (a, s))}
<
< instance (Monad m) => Monad (StateT s m) where
<   return a = StateT $ \s -> return (a, s)
<   (StateT x) >>= f = StateT $ \s ->
<     (val, s') <- x s
<     in runStateT (f val) s'
<
< instance (Monad m) => MonadState s (StateT s m) where
<   get = StateT $ \s -> return (s, s)  -- Get the passed-in state, while leaving
<                                       -- the state unchanged
<   put s = StateT $ \_ -> return ((), s)  -- Set state as s, disregarding the
<                                          -- passed-in state


> main :: IO ()
> main = do
>   hspec $ do
>     describe "Check passphrase" $ do
>       it "should reject alphabet-only passwords" $ do
>         writeIORef getLineRef "onlyalphabet"
>         passphrase <- runMaybeT getValidPassphrase
>         passphrase `shouldBe` Nothing
>       it "should reject numeric-only passwords" $ do
>         writeIORef getLineRef "135797531"
>         passphrase <- runMaybeT getValidPassphrase
>         passphrase `shouldBe` Nothing
>       it "should reject punc-only passwords" $ do
>         writeIORef getLineRef "-[;.=]]'['.\"\"']'"
>         passphrase <- runMaybeT getValidPassphrase
>         passphrase `shouldBe` Nothing
>       it "should reject passwords too short" $ do
>         writeIORef getLineRef "Dd3["
>         passphrase <- runMaybeT getValidPassphrase
>         passphrase `shouldBe` Nothing
>       it "should accept valid passwords" $ do
>         writeIORef getLineRef "DDDDDDDdddddddd3333333]]]]]]]"
>         passphrase <- runMaybeT getValidPassphrase
>         passphrase `shouldBe` (Just "DDDDDDDdddddddd3333333]]]]]]]")
>   return ()

Not so intuitive right? I'm still quite confused and not sure how much it can
help simplify my code.
