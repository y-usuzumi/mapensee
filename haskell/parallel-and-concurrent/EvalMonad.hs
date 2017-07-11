module Main where

import           Control.Parallel.Strategies
import           Text.Printf

factorial :: Int -> Integer
factorial n = foldl (*) 1 [1..(toInteger n)]

testPar :: Int -> Integer
testPar n = do
  sum $ parMap rpar factorial [1..n]

main :: IO ()
main = do
  printf "Sum of all factorials of [1..1000] is: %ld" $ testPar 1000
