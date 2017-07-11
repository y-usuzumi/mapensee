module LinkUp where

import           Control.Monad
import           Data.List
import qualified Data.Vector   as V
import           System.Random

type Pattern = Int
data Block = Empty | Pattern Pattern
type Size = (Int, Int)
instance Show Block where
  show Empty       = " "
  show (Pattern p) = show p
type Board = V.Vector (V.Vector Block)

printBoard :: Board -> IO ()
printBoard board = forM_ board $ \row -> do
  putStrLn $ printBoardRow row
  where
    printBoardRow row = intercalate " " $ map show (V.toList row)

randomizeBoard :: Size -> IO Board
randomizeBoard (rowSize, colSize) =
  replicateM rowSize randomizeRow >>= return . V.fromList
  where
    randomizeRow = replicateM colSize (randomRIO (0, 9) >>= return . numToBlock) >>= return . V.fromList
    numToBlock 0 = Empty
    numToBlock p = Pattern p


main :: IO ()
main = do
  board <- randomizeBoard (8, 10)
  printBoard board
