{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad
import qualified Data.ByteString.Char8 as C8
import Data.List
import System.ZMQ4.Monadic
import Text.Printf

average :: [Double] -> Double
average = (/) <$> sum <*> genericLength

main :: IO ()
main =
  runZMQ $ do
    subscriber <- socket Sub
    connect subscriber "tcp://localhost:5556"
    subscribe subscriber "10001"
    liftIO $ putStrLn "Starting weather broadcast client..."
    records <- replicateM 5 $ do
      liftIO $ putStrLn "Got a message"
      update <- receive subscriber
      let [_, temp, hum] = map read $ words $ C8.unpack update
      return (temp, hum)
    liftIO $ printf "NY City: avg temperature of %.1f°C / avg humidity of %.1f%%\n"
      (avgTemp records) (avgHum records)
    return ()
      where
        avgTemp r = average $ map fst r
        avgHum r = average $ map snd r
