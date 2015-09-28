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
> import Control.Monad
> import Control.Monad.Trans
> import Control.Monad.Trans.Maybe

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

> isValid :: String -> Bool
> isValid s =
>   length s >= 8
>   && any isAlpha s
>   && any isNumber s
>   && any isPunctuation s
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
> main :: IO ()
> main = hspec $ do
>   -- Don't know how to test IO yet.
>   return ()

Not so intuitive right? I'm still quite confused and not sure how much it can
help simplify my code.

