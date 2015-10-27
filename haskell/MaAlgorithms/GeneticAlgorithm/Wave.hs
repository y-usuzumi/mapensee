{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- 遗传算法
-- 目标：求解z = (0.5-(sin(sqrt(x**2+y**2))**2-0.5)/(1+(0.001*(x**2+y**2))**2))的最大值


import Control.Monad
import Control.Monad.Loops
import Control.Monad.Random
import Control.Monad.ST
import Data.Bits
import Data.Function
import Data.IORef
import Data.List
import Data.STRef
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.Word

-- 基因类型
data Gene dtype where
  MkGene :: (FiniteBits dtype, Bounded dtype) => dtype -> Gene dtype

-- 由于使用了GADTs，直接deriving Show是不行的。。
deriving instance Show (dtype) => Show (Gene dtype)

-- 基因组类型。分别用geneX和geneY对应X坐标和Y坐标的基因表示
data Genome dtype = MkGenome {
  geneX :: Gene dtype,
  geneY :: Gene dtype
  } deriving Show

-- 交叉概率、变异概率等的表示
type Rate = Double

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


crossover :: (MonadRandom m, Bounded dtype) => Genome dtype -> Genome dtype -> Rate -> m (Genome dtype, Genome dtype)  -- Crossover makes two offsprings from two parents
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

mutate :: (MonadRandom m, Bounded dtype) => Genome dtype -> Rate -> m (Genome dtype)
mutate MkGenome{..} prob = do
  [rnd0, rnd1] <- take 2 <$> getRandomRs (0.0, 1.0)
  newGeneX <- if prob >= rnd0 then mutateGene geneX else return geneX
  newGeneY <- if prob >= rnd1 then mutateGene geneY else return geneY
  return MkGenome { geneX=newGeneX, geneY=newGeneY }

-- 以下是专门用于求解本问题的非通用代码

type Word64Gene = Gene Word64
type Word64Genome = Genome Word64

genomeToCoord :: Word64Genome -> (Double, Double)
genomeToCoord MkGenome {..} =
  (geneToAxis geneX, geneToAxis geneY)
  where
    geneToAxis :: Word64Gene -> Double
    geneToAxis (MkGene d) = let
      numD = fromIntegral d
      numMaxBound = fromIntegral (maxBound :: Word64)
      in
        numD / numMaxBound * 20 - 10

assess :: Word64Genome -> Double
assess genome = let
  (x, y) = genomeToCoord genome
  in
  (0.5 - (sin (sqrt (x**2 + y**2))**2 - 0.5) / (1 + (0.001 * (x**2 + y**2)**2)))

-- 自动生成原始人口
populate :: forall m. (MonadRandom m) => Int -> m (V.Vector Word64Genome)
populate num =
  V.replicateM num populateOne
  where
    populateOne :: m Word64Genome
    populateOne = do
      [gx, gy] <- replicateM 2 $ getRandomR (minBound, maxBound)
      return MkGenome {geneX=MkGene gx, geneY=MkGene gy}

-- Check it out with map genomeToCoord <$> populate 3 :)

select :: (MonadRandom m) => V.Vector Word64Genome -> Int -> m (V.Vector Word64Genome)
select genomes num = do
  let assessments = V.map assess genomes
  thres <- fmap V.fromList $ take num <$> getRandomRs (0, sum assessments)
  let gapairs = V.zip genomes assessments
  return $ V.map (`pick` gapairs) thres
  where
    pick :: Double -> V.Vector (Word64Genome, Double) -> Word64Genome
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

data Playground = Playground {
  crossoverRate :: Double,
  mutateRate :: Double,
  maxPopulation :: Int,
  maxGens :: Int
  }

mkNextGen :: (MonadRandom m) => Playground -> V.Vector Word64Genome -> m (V.Vector Word64Genome)
mkNextGen Playground{..} genomes = do
  -- Retain the best one, drop others
  let best = V.maximumBy (compare `on` assess) genomes
  candidates <- select genomes maxPopulation
  bpc <- batchPairCrossover candidates
  return $ V.init bpc `V.snoc` best
  where
    batchPairCrossover :: (MonadRandom m) => V.Vector Word64Genome -> m (V.Vector Word64Genome)
    batchPairCrossover candidates = do
      offspringPairs <- V.mapM (\idx -> crossover (candidates V.! idx) (candidates V.! (idx+1)) crossoverRate) (V.fromList [0,2..maxPopulation - 2])
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

defaultPlayground :: Playground
defaultPlayground = Playground {
    crossoverRate = 0.5,
    mutateRate = 0.3,
    maxPopulation = 1600,
    maxGens = 1000
    }

main :: IO ()
main = do
  let playground = defaultPlayground
  initialGen <- populate $ maxPopulation playground
  genRef <- newIORef initialGen
  idxRef <- newIORef 0
  (_, lastGen) <- iterateUntil ((>= maxGens playground) . fst) (
    do
      idx <- readIORef idxRef
      gen <- readIORef genRef
      putStrLn $ "Gen " ++ show (idx + 1) ++ ":"
      print $ sum $ V.map assess gen
      nextGen <- mkNextGen playground gen
      writeIORef genRef nextGen
      modifyIORef idxRef (+1)
      return (idx, nextGen)
    )
  let bestChoice = V.maximumBy (compare `on` assess) lastGen
  putStrLn $ "Best choice assessed as: " ++ show (assess bestChoice)
  putStrLn $ "Final result: " ++ show (genomeToCoord bestChoice)
