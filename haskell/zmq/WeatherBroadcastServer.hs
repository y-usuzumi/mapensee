{-# LANGUAGE ExtendedDefaultRules #-}

module Main where

import Control.Monad
import System.ZMQ4.Monadic
import qualified Data.ByteString.Char8 as C8
import System.Random
import Text.Printf

{- WARNING! This program will eat up one core! -}
main :: IO ()
main =
  runZMQ $ do
    publisher <- socket Pub
    bind publisher "tcp://*:5556"
    bind publisher "ipc://weather.ipc"
    liftIO $ putStrLn "Starting weather broadcast server..."
    forM_ [1..] $ \t -> do
      zipCode <- liftIO $ randomRIO (0, 100000)
      temperature <- liftIO $ randomRIO (-80, 135)
      humidity <- liftIO $ randomRIO (10, 60)
      let update = C8.pack $ unwords $ map show [zipCode, temperature, humidity]
      when (t `rem` 100000 == 0) $
        liftIO $ putStrLn $ printf "%d msgs sent" t
      send publisher [] update
