{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}

module GAKit where

import Data.Bits
import Data.List
import Control.Monad
import Control.Monad.Random
import GHC.Prim

data Gene dtype where
  Gene { geneToData :: (FiniteBits dtype, Bounded dtype) => dtype } :: Gene dtype

data Genome dtype = Genome {
  geneX :: Gene dtype,
  geneY :: Gene dtype
  }

type Probability = Double

genomeToData :: (FiniteBits dtype, Bounded dtype) => Genome dtype -> (dtype, dtype)
genomeToData Genome{..}
  | (Gene gx) <- geneX,
    (Gene gy) <- geneY
                 = (gx, gy)

type CrossoverSelector m = (Monad m) => Genome dtype -> Genome dtype -> m Bool

type CrossoverAlgorithm m = (Monad m, FiniteBits dtype, Bounded dtype) => Gene dtype -> Gene dtype -> m (Gene dtype, Gene dtype)

simpleCrossoverSelector :: MonadRandom m => Probability -> CrossoverSelector m
simpleCrossoverSelector prob father mother = do
  rand <- getRandomR (0, 1)
  return $ rand > prob

simpleCrossoverAlgorithm :: MonadRandom m => CrossoverAlgorithm m
simpleCrossoverAlgorithm father mother = do
  let (fd, md) = (geneToData father, geneToData mother)
  let fbs = finiteBitSize fd
  slicePos1:slicePos2:[] <- sort <$> (replicateM 2 $ getRandomR (0, finiteBitSize fd - 1))
  let maskTwoSides = shiftR maxBound (fbs - slicePos2 - 1) .&. (shiftL maxBound slicePos1)
  let maskMid = complement maskTwoSides
  return (
    Gene $ fd .&. maskMid .|. md .&. maskTwoSides,
    Gene $ md .&. maskMid .|. fd .&. maskTwoSides
    )

crossover :: (Monad m) =>
             Genome dtype
             -> Genome dtype
             -> CrossoverSelector m
             -> CrossoverAlgorithm m
             -> m (Genome dtype, Genome dtype)
crossover father mother selector algorithm
  | shouldCrossover <- selector father mother
  , shouldCrossover = algorithm father mother
  
