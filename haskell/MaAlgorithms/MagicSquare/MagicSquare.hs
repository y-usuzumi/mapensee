{-# LANGUAGE RecordWildCards #-}

module MagicSquare where

import           Control.Monad
import           Control.Monad.Random
import           Data.Maybe
import qualified Data.Vector          as V
import qualified Data.Vector.Mutable  as MV
import           System.Environment
import GHC.Arr
import Data.Array.ST
import Data.Function
import Data.List
import Data.List.Split
import Statistics.Sample

newtype Gene = MkGene (V.Vector Int) deriving Show

type Rate = Double

selectRandomRange :: MonadRandom m => Int -> m (Int, Int)
selectRandomRange size = do
  (r1:r2:_) <- getRandomRs (0, size - 1)
  return $ if r1 < r2 then (r1, r2) else (r2, r1)

crossover :: MonadRandom m => Gene -> Gene -> m Gene
crossover (MkGene father) (MkGene mother) = do
  let upperBound = V.length father - 1
  rnd <- getRandomR (0, upperBound)
  let child_ = V.take rnd father
  return $ MkGene (child_ V.++ V.filter (`notElem` child_) mother)

mutate :: MonadRandom m => Gene -> m Gene
mutate (MkGene gene) = do
  (l, r) <- selectRandomRange (V.length gene)
  return $ MkGene (gene V.// [(l, gene V.! r), (r, gene V.! l)])


data PlayGround = PlayGround { crossoverRate :: Double
                             , mutateRate :: Double
                             , maxPopulation :: Int
                             , maxGens :: Int
                             , squareSize :: Int
                             }

type Generation = V.Vector Gene

shuffle :: MonadRandom m => [a] -> m [a]
shuffle xs = do
    let l = length xs
    rands <- take l `fmap` getRandomRs (0, l-1)
    let ar = runSTArray $ do
        ar <- thawSTArray $ listArray (0, l-1) xs
        forM_ (zip [0..(l-1)] rands) $ \(i, j) -> do
            vi <- readSTArray ar i
            vj <- readSTArray ar j
            writeSTArray ar j vi
            writeSTArray ar i vj
        return ar
    return (elems ar)

populate :: MonadRandom m => PlayGround -> Int -> m Generation
populate PlayGround {..} num =
  V.replicateM num populateOne
  where
    populateOne = do
      l <- shuffle [1..squareSize]
      return $ MkGene (V.fromList l)

select :: MonadRandom m => PlayGround -> Generation -> m Generation
select = undefined

assess :: Gene -> Double
assess (MkGene gene) = let
  squareSize = round (sqrt (fromIntegral $ V.length gene))
  geneL = V.toList gene
  rowCols = splitEvery squareSize geneL
  colRows = transpose rowCols
  sumRows = map sum rowCols
  sumCols = map sum colRows
  sumDiags = map sum [ every (squareSize+1) geneL
                     , every (squareSize-1) (drop (squareSize-1) geneL)
                     ]
  in
    
  where
    every _ [] = []
    every n l@(x:xs) = x:every n (drop n l)

mkNextGen :: MonadRandom m => PlayGround -> Generation -> m Generation
mkNextGen PlayGround {..} genes = do
  let best = V.minimumBy (compare `on` assess) genes
  undefined
