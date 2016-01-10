import Control.Concurrent
import Data.IORef
import System.IO.Unsafe
import System.Posix.Signals
import System.Random

swapMed :: IORef Int
{-# NOINLINE swapMed #-}
swapMed = unsafePerformIO $ newIORef 0

swap :: (Int, Int) -> IO (Int, Int)
swap (a, b) = do
  sleepSeconds <- getStdRandom (randomR (1, 3))
  writeIORef swapMed a
  threadDelay $ sleepSeconds * 1000000
  let a' = b
  swapMed' <- readIORef swapMed
  let b' = swapMed'
  return (a', b')

kbdHandler :: IO ()
kbdHandler = do
  ret <- swap (3, 4)
  putStrLn $ "Swap of (3,4) is: " ++ show ret
  return ()

main :: IO ()
main = do
  _ <- installHandler keyboardSignal (Catch kbdHandler) Nothing
  ret <- swap (1, 2)
  putStrLn $ "Swap of (1,2) is: " ++ show ret
  threadDelay 5000000
