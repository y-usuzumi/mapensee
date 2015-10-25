{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}

-- 遗传算法
-- 目标：求解z = (0.5-(sin(sqrt(x**2+y**2))**2-0.5)/(1+(0.001*(x**2+y**2))**2))的最大值


import Data.Bits
import Data.List
import Data.STRef
import Control.Monad
import Control.Monad.Random
import Control.Monad.ST

data Gene dtype where
  MkGene :: (FiniteBits dtype) => dtype -> Gene dtype

deriving instance Show (dtype) => Show (Gene dtype)

data Genome dtype = MkGenome {
  geneX :: Gene dtype,
  geneY :: Gene dtype
  } deriving Show

type Probability = Double  -- The probability that crossover happens. [0.0, 1.0]

selectRandomRange :: (MonadRandom m, FiniteBits dtype) => dtype -> m (Int, Int)
selectRandomRange d = do
  [left, right] <- sort <$> (take 2 <$> getRandomRs (0, finiteBitSize d -1))
  return (left, right)

crossoverGene :: (MonadRandom m, Bounded dtype) => Gene dtype -> Gene dtype -> m (Gene dtype, Gene dtype)
crossoverGene (MkGene father) (MkGene mother) = do
  (left, right) <- selectRandomRange father
  let mask = shiftR (shiftL maxBound (left + finiteBitSize father - right - 1)) left
  let counterMask = complement mask
  return (MkGene (father .&. counterMask .|. mother .&. mask), MkGene (mother .&. counterMask .|. father .&. mask))


crossover :: (MonadRandom m, Bounded dtype) => Genome dtype -> Genome dtype -> Probability -> m (Genome dtype, Genome dtype)  -- Crossover makes two offsprings from two parents
crossover father mother prob = do
  rnd <- getRandomR (0.0, 1.0)
  if prob >= rnd then
    do
      let (fx, fy) = (geneX father, geneY father)
      let (mx, my) = (geneX mother, geneY mother)
      (fx', mx') <- crossoverGene fx mx
      (fy', my') <- crossoverGene fy my
      return (MkGenome fx' fy', MkGenome mx' my')
    else return (father, mother)

mutateGene :: (MonadRandom m, Bounded dtype) => Gene dtype -> m (Gene dtype)
mutateGene (MkGene gene) = do
  (left, right) <- selectRandomRange gene
  return $ MkGene $ runST $ do
    stRef <- newSTRef gene
    forM_ [left..right] $ \idx ->
      readSTRef stRef >>= writeSTRef stRef . flip complementBit idx
    readSTRef stRef

mutate :: (MonadRandom m, Bounded dtype) => Genome dtype -> Probability -> m (Genome dtype)
mutate MkGenome{..} prob = do
  [rnd0, rnd1] <- take 2 <$> getRandomRs (0.0, 1.0)
  newGeneX <- if prob >= rnd0 then mutateGene geneX else return geneX
  newGeneY <- if prob >= rnd1 then mutateGene geneY else return geneY
  return MkGenome { geneX=newGeneX, geneY=newGeneY }

genomeToCoord :: Genome dtype -> (Int, Int)
genomeToCoord MkGenome {..} =
  (geneToAxis geneX, geneToAxis geneY)
  where
    geneToAxis (MkGene d) = if d < 0 then (d / minBound * 10) else (d / maxBound * 10)

assessmentFunction :: Genome dtype -> Int
assessmentFunction genome = let
  (x, y) = genomeToCoord genome
  in
  (0.5 - (sin (sqrt (x**2 + y**2))**2 - 0.5) / (1 + (0.001 * (x**2 + y**2)**2)))

main :: IO ()
main = undefined
