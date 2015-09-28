We have seen how monads can help handling IO actions, Maybe, lists, and state. With monads providing a common way to use such useful general-purpose tools, a natural thing we might want to do is using the capabilities of several monads at once. For instance, a function could use both I/O and Maybe exception handling. While a type like IO (Maybe a) would work just fine, it would force us to do pattern matching within IO do-blocks to extract values, something that the Maybe monad was meant to spare us from.

Enter monad transformers: special types that allow us to roll two monads into a single one that shares the behavior of both.

> module Main where
>
> import Test.Hspec
> import Control.Monad
> import Control.Monad.Trans.Maybe

Definition of MaybeT:
newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a) }

> -- The signature of (>>=)
> main :: IO ()
> main = hspec $ do
>   return ()
