{-# LANGUAGE RecordWildCards #-}

module Main where

import           Control.Monad
import Control.Monad.Loops
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
import Control.Monad.ST
import Data.STRef
import Data.IORef

newtype Gene = MkGene (V.Vector Int)

instance Show Gene where
  show (MkGene gene) = let
    squareSize = round (sqrt (fromIntegral $ V.length gene) :: Double)
    matrix = chunksOf squareSize (V.toList gene)
    in unlines $ map (\row -> intercalate "\t" (map show row)) matrix

type Rate = Double

selectRandomRange :: MonadRandom m => Int -> m (Int, Int)
selectRandomRange size = do
  (r1:r2:_) <- getRandomRs (0, size - 1)
  return $ if r1 < r2 then (r1, r2) else (r2, r1)

crossover :: MonadRandom m => PlayGround -> Gene -> Gene -> m (Gene, Gene)
crossover PlayGround {..} gf gm = do
  rnd <- getRandomR (0, 1)
  if rnd < crossoverRate then _crossover gf gm else return (gf, gm)
  where
  _crossover :: MonadRandom m => Gene -> Gene -> m (Gene, Gene)
  _crossover (MkGene father) (MkGene mother) = do
    let upperBound = V.length father - 1
    rnd <- getRandomR (0, upperBound)
    let child_1 = V.take rnd father
        child_2 = V.take rnd mother
    return ( MkGene (child_1 V.++ V.filter (`notElem` child_1) mother)
           , MkGene (child_2 V.++ V.filter (`notElem` child_2) father)
           )

mutate :: MonadRandom m => PlayGround -> Gene -> m Gene
mutate PlayGround {..} g = do
  rnd <- getRandomR (0.0, 1.0)
  if rnd < mutateRate then _mutate g else return g
  where
    _mutate :: MonadRandom m => Gene -> m Gene
    _mutate (MkGene gene) = do
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

populate :: MonadRandom m => PlayGround -> m Generation
populate PlayGround {..} =
  V.replicateM maxPopulation populateOne
  where
    populateOne = do
      l <- shuffle [1..squareSize * squareSize]
      return $ MkGene (V.fromList l)

select :: MonadRandom m => PlayGround -> Generation -> m Generation
select PlayGround {..} genes = do
  let assessments = V.map assess genes
  thres <- fmap V.fromList $ take maxPopulation <$> getRandomRs (0, sum assessments)
  let gapairs = V.zip genes assessments
  return $ V.map (`pick` gapairs) thres
  where
    pick :: Double -> V.Vector (Gene, Double) -> Gene
    pick thres gapairs = runST $ do
      idxRef <- newSTRef 0
      currRef <- newSTRef 0.0
      (_, genome) <- iterateUntil ((> thres) . fst) $ do
        idx <- readSTRef idxRef
        curr <- readSTRef currRef
        let (genome, assessment) = gapairs V.! idx
        modifySTRef' idxRef (+1)
        let newCurr = curr + assessment
        writeSTRef currRef newCurr
        return (newCurr, genome)
      return genome

assess :: Gene -> Double
assess (MkGene gene) = let
  squareSize = round (sqrt (fromIntegral $ V.length gene) :: Double)
  geneL = V.toList gene
  rowCols = chunksOf squareSize geneL
  colRows = transpose rowCols
  sumRows = map sum rowCols
  sumCols = map sum colRows
  sumDiags = map sum [ every (squareSize+1) geneL
                     , init $ every (squareSize-1) (drop (squareSize-1) geneL)
                     ]
  in
    1 / (stdDev (V.fromList $ map fromIntegral (sumRows ++ sumCols ++ sumDiags)) + 0.0001)
  where
    every _ [] = []
    every n l@(x:_) = x:every n (drop n l)

mkNextGen :: MonadRandom m => PlayGround -> Generation -> m Generation
mkNextGen pg@PlayGround {..} genes = do
  let best = V.maximumBy (compare `on` assess) genes
  candidates <- select pg genes
  offsprings <- batchPairCrossover candidates
  mutatedOffsprings <- V.mapM (mutate pg) offsprings
  return $ V.init mutatedOffsprings `V.snoc` best
  where
    batchPairCrossover :: MonadRandom m => Generation -> m Generation
    batchPairCrossover candidates = do
      offspringPairs <- V.mapM (\idx -> crossover pg (candidates V.! idx) (candidates V.! (idx+1))) (V.fromList [0,2..maxPopulation - 2])
      return $ V.create $ do
        v <- MV.new maxPopulation
        idxRef <- newSTRef 0
        _ <- iterateWhile (< maxPopulation `quot` 2) (
          do
            idx <- readSTRef idxRef
            let (os1, os2) = offspringPairs V.! idx
            MV.write v (2*idx) os1
            MV.write v (2*idx+1) os2
            modifySTRef' idxRef (+1)
            return (idx+1)
          )
        return v

main :: IO ()
main = do
  (sizeStr:_) <- getArgs
  let playGround = PlayGround { crossoverRate = 0.5
                              , mutateRate = 0.5
                              , maxPopulation = 1000
                              , maxGens = 500
                              , squareSize = read sizeStr :: Int
                              }
  initialGen <- populate playGround
  genRef <- newIORef initialGen
  idxRef <- newIORef 0
  (_, lastGen) <- iterateUntil ((>= maxGens playGround) . fst) (
    do
      idx <- readIORef idxRef
      gen <- readIORef genRef
      -- putStrLn $ "Gen " ++ show (idx + 1) ++ ":"
      -- print $ (sum (V.map assess gen) / fromIntegral (V.length gen))
      nextGen <- mkNextGen playGround gen
      writeIORef genRef nextGen
      modifyIORef idxRef (+1)
      return (idx, nextGen)
    )
  let bestChoice = V.maximumBy (compare `on` assess) lastGen
  putStrLn $ "Best choice assessed as: " ++ show (assess bestChoice)
  putStrLn $ "Final result:\n" ++ show (bestChoice)
