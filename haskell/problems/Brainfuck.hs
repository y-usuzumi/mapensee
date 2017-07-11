{-# LANGUAGE RecordWildCards #-}

module Brainfuck
    ( executeString
    ) where

import           Control.Monad.ST
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as C8
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           Data.Word
import           Debug.Trace

data Env = Env { instrs   :: !(V.Vector Char)
               , instrptr :: !Int
               , mem      :: !(V.Vector Word8)
               , memptr   :: !Int
               , input    :: [Word8]
               , output   :: [Word8]
               } deriving Show

modifyVector :: (MV.Unbox a) => V.Vector a -> Int -> (a -> a) -> V.Vector a
modifyVector v idx f = runST $ do
  v' <- V.thaw v
  MV.write v' idx (f (v V.! idx))
  V.freeze v'

runInstr :: Env -> Maybe Env
runInstr env@Env{..} = let
  instr = instrs V.! instrptr
  in
    case instr of
      '>' -> Just env { memptr = memptr+1
                      , instrptr = instrptr+1
                      }
      '<' -> Just env { memptr = memptr-1
                      , instrptr = instrptr+1
                      }
      '+' -> Just env { mem = modifyVector mem memptr (+1)
                      , instrptr = instrptr+1
                      }
      '-' -> Just env { mem = modifyVector mem memptr (subtract 1)
                      , instrptr = instrptr+1
                      }
      '.' -> Just env { output = output ++ [mem V.! memptr]
                      , instrptr = instrptr+1
                      }
      ',' -> case input of
        [] -> Nothing
        _ -> Just env { mem = modifyVector mem memptr (const (head input))
                      , input = tail input
                      , instrptr = instrptr+1
                      }
      '[' -> Just env { instrptr = jz env }
      ']' -> Just env { instrptr = jnz env }
  where
    jz :: Env -> Int
    jz env@Env {..}
      | mem V.! memptr == 0 = findMatchingJNZ instrs (instrptr + 1)
      | otherwise = instrptr+1
    jnz :: Env -> Int
    jnz env@Env {..}
      | mem V.! memptr /= 0 = findMatchingJZ instrs (instrptr - 1)
      | otherwise = instrptr+1
    findMatchingJNZ :: V.Vector Char -> Int -> Int
    findMatchingJNZ = findMatchingJNZ_ 1
      where
        findMatchingJNZ_ 0 instrs instrptr = instrptr
        findMatchingJNZ_ n instrs instrptr = case instrs V.! instrptr of
          '[' -> findMatchingJNZ_ (n+1) instrs (instrptr+1)
          ']' -> findMatchingJNZ_ (n-1) instrs (instrptr+1)
          _ -> findMatchingJNZ_ n instrs (instrptr+1)
    findMatchingJZ :: V.Vector Char -> Int -> Int
    findMatchingJZ = findMatchingJZ_ 1
      where
        findMatchingJZ_ 0 instrs instrptr = instrptr + 2  -- Move memptr from the left of JZ to the right
        findMatchingJZ_ n instrs instrptr = case instrs V.! instrptr of
          ']' -> findMatchingJZ_ (n+1) instrs (instrptr-1)
          '[' -> findMatchingJZ_ (n-1) instrs (instrptr-1)
          _ -> findMatchingJZ_ n instrs (instrptr-1)

run :: Env -> Maybe Env
run env@Env {..}
  | instrptr < V.length instrs = runInstr env >>= run
  | otherwise = return env

-- | Interprets the Brainfuck source code from the first argument, while
-- supplying it with input from the second. May fail on insufficient input.
executeString :: String -> String -> Maybe String
executeString source input = do
  let instrs = V.fromList source
      input' = BS.unpack $ C8.pack input
  env' <- run $ Env { instrs = instrs
                    , instrptr = 0
                    , mem = V.replicate 2048 0
                    , memptr = 0
                    , input = input'
                    , output = []
                    }
  return $ C8.unpack $ BS.pack $ output env'
