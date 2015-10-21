import System.IO
import qualified Data.List as L
import qualified Data.List.Split as LS
import AI.HNN.FF.Network
import Numeric.LinearAlgebra

createSineNetwork :: IO (Network Double)
createSineNetwork = createNetwork 2 [4, 4] 1

samplesFromTrainingSet :: [[String]] -> Samples Double
samplesFromTrainingSet =
  map sampleFromTrainingRow
  where
  sampleFromTrainingRow x = let
    xDouble = map read x
    in
    (fromList (init x), fromList [tail x])

main :: IO ()
main = do
  rawData <- readFile "trainingset"
  let trainingSet = map (LS.splitOn ",") $ lines rawData
  network <- createSineNetwork
  let samples = samplesFromTrainingSet trainingSet
  print samples
