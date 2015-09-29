> module Main where
>
> import Control.Concurrent

The following will print GG and HH interleaved because putStrLn is not atomic.
< main :: IO ()
< main = do
<   forkIO $ do
<     putStrLn "Child"
<   putStrLn "Parent"

> main :: IO ()
> main = do
>   mutex <- newMVar ()
>   tid <- forkIO $ do
>     withMVar mutex $ \_ ->
>       putStrLn "Child"
>   withMVar mutex $ \_ -> do
>     putStrLn $ show tid
>     putStrLn "Parent"
