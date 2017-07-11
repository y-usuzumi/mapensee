{- Perceptron

1. For every input, multiply that input by its weight.
2. Sum all of the weighted inputs.
3. Comput the output of the perceptron based on that sum
   passed through an activation function.

-}

{-# LANGUAGE GADTs              #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE RankNTypes         #-}

import           System.Random

data PerceptronF i w o where
  PerceptronF :: { weighInput :: i -> w -> wi  -- function that applies to an input and a weight
                 , sumInputs :: [b] -> c  -- function that aggregates all weighted inputs
                 , activation :: c -> o  -- function that turns the summed inputs into the output
                 } -> PerceptronF i w o

data Perceptron i w o = Perceptron (PerceptronF i w o) [w]

type BasicPerceptronF a = Num a => PerceptronF a a a
type BasicPerceptron a = Num a => Perceptron a a a

basicPerceptronF :: BasicPerceptronF a
basicPerceptronF = PerceptronF { weighInput = (*)
                               , sumInputs = sum
                               , activation = signum
                               }

basicPerceptronFromWeights :: [a] -> BasicPerceptron a
basicPerceptronFromWeights = Perceptron basicPerceptronF

randomWeights :: Random w => (w, w) -> IO [w]
randomWeights range = randomRs range <$> getStdGen

randomPerceptron :: Random w => (w, w) -> PerceptronF i w o -> IO (Perceptron i w o)
randomPerceptron range perceptronF = Perceptron perceptronF <$> randomWeights range


-- FIXME: Not working. Impredicative types.
randomBasicPerceptron :: Random a => IO (BasicPerceptron a)
randomBasicPerceptron = basicPerceptronFromWeights <$> randomWeights (-1, 1)
