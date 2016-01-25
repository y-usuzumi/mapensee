{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# LANGUAGE RecordWildCards   #-}

module Befunge93 where

import           Control.Monad
import           Control.Monad.Loops
import           Control.Monad.ST
import           Control.Monad.Trans.State
import           Data.Char
import qualified Data.Text                   as T
import qualified Data.Vector                 as V
import qualified Data.Vector.Unboxed         as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import qualified System.Random               as R

pattern Add = '+'
pattern Sub = '-'
pattern Mul = '*'
pattern Div = '/'
pattern Mod = '%'
pattern Not = '!'
pattern Gt = '`'
pattern R = '>'
pattern L = '<'
pattern U = '^'
pattern D = 'v'
pattern RndDir = '?'
pattern PopLR = '_'
pattern PopUD = '|'
pattern Str = '"'
pattern Dup = ':'
pattern Swap = '\\'
pattern Discard = '$'
pattern Out = '.'
pattern OutChr = ','
pattern Tramp = '#'
pattern Put = 'p'
pattern Get = 'g'
pattern End = '@'
pattern NoOp = ' '

type Instr = Char
type Instrs = V.Vector (UV.Vector Instr)
type Pointer = (Int, Int)
type Stack = [Int]
data NoType

data VM = VM { instrs        :: Instrs
             , stack         :: Stack
             , instrptr      :: {-# UNPACK #-} !Pointer
             , currDirection :: {-# UNPACK #-} !Instr
             , outputBuf     :: T.Text
             , stdGen        :: R.StdGen
             }

-------------------
-- Stack operations
-------------------

pushStack :: Int -> State VM ()
pushStack i = state $ \vm@VM {..} -> ((), vm { stack = i:stack })

popStack :: State VM Int
popStack = state $ \vm@VM {..} -> let
  (x:xs) = stack
  in (x, vm { stack = xs })

popStackSafe :: State VM Int
popStackSafe = state $ \vm@VM {..} -> case stack of
  [] -> (0, vm)
  (x:xs) -> (x, vm { stack = xs })

---------
-- Output
---------

output :: Char -> State VM ()
output char = state $ \vm@VM {..} -> ((), vm { outputBuf = T.snoc outputBuf char })

outputS :: String -> State VM ()
outputS str = state $ \vm@VM {..} -> ((), vm { outputBuf = T.append outputBuf (T.pack str) })

----------------------------
-- Instruction pointer moves
----------------------------

move :: State VM ()
move = state $ \vm@VM {..} -> let
  diff = case currDirection of
    U -> (-1, 0)
    D -> (1, 0)
    L -> (0, -1)
    R -> (0, 1)
    _ -> error "No such direction"
  in
    ((), vm { instrptr = applyDiff instrptr diff})
  where
    applyDiff :: Pointer -> Pointer -> Pointer
    applyDiff (l1, l2) (d1, d2) = (l1+d1, l2+d2)

setDirection :: Instr -> State VM ()
setDirection direction = state $ \vm@VM {..} -> ((), vm { currDirection = direction })

-------------------------
-- Instruction read/write
-------------------------

getInstruction :: State VM Instr
getInstruction = state $ \vm@VM {..} -> let
  (a1, a2) = instrptr
  in
    (instrs V.! a1 UV.! a2, vm)

getInstructionAt :: Pointer -> State VM Instr
getInstructionAt (a1, a2) = state $ \vm@VM {..} -> (instrs V.! a1 UV.! a2, vm)

setInstructionAt :: Pointer -> Instr -> State VM ()
setInstructionAt pos instr = state $ \vm@VM {..} -> ((), vm { instrs = runST $ do
  let v = instrs V.! fst pos
  v' <- UV.thaw v
  UMV.write v' (snd pos) instr
  _ <- UV.freeze v'
  return instrs
  })

---------
-- StdGen
---------

getStdGen :: State VM R.StdGen
getStdGen = state $ \vm@VM {..} -> (stdGen, vm)

setStdGen :: R.StdGen -> State VM ()
setStdGen sg = state $ \vm@VM {..} -> ((), vm { stdGen = sg })

------
-- Run
------

runInstr :: Instr -> State VM ()
runInstr instr
  | instr `elem` ['0'..'9'] = pushStack (digitToInt instr) >> move
  | instr `elem` [U,D,L,R] = setDirection instr >> move
  | otherwise = case instr of
      Add -> do
        a <- popStackSafe
        b <- popStackSafe
        pushStack (a+b)
        move
      Sub -> do
        a <- popStackSafe
        b <- popStackSafe
        pushStack (b-a)
        move
      Mul -> do
        a <- popStackSafe
        b <- popStackSafe
        pushStack (a*b)
        move
      Div -> do
        a <- popStackSafe
        b <- popStackSafe
        pushStack $ if a == 0 then 0 else b `quot` a
        move
      Mod -> do
        a <- popStackSafe
        b <- popStackSafe
        pushStack $ if a == 0 then 0 else b `rem` a
        move
      Not -> do
        a <- popStackSafe
        pushStack $ if a == 0 then 1 else 0
        move
      Gt -> do
        a <- popStackSafe
        b <- popStackSafe
        pushStack $ if b > a then 1 else 0
        move
      RndDir -> do
        stdGen <- getStdGen
        let (a, g) = R.randomR (0, 3) stdGen
        setDirection ("UDLR" !! a)
        setStdGen g
        move
      PopLR -> do
        a <- popStackSafe
        if a == 0
          then setDirection R >> move
          else setDirection L >> move
      PopUD -> do
        a <- popStackSafe
        if a == 0
          then setDirection D >> move
          else setDirection U >> move
      Str -> move >> unfoldM_ (do
        a <- getInstruction
        if a /= Str
          then pushStack (ord a) >> move >> return (Just ())
          else return Nothing) >> move
      Dup -> do
        a <- popStackSafe
        pushStack a
        pushStack a
        move
      Swap -> do
        a <- popStackSafe
        b <- popStackSafe
        pushStack a
        pushStack b
        move
      Discard -> popStackSafe >> move
      Out -> do
        a <- popStackSafe
        outputS (show a)
        move
      OutChr -> do
        a <- popStackSafe
        output (chr a)
        move
      Tramp -> move >> move
      Put -> do
        y <- popStackSafe
        x <- popStackSafe
        v <- popStackSafe
        setInstructionAt (x, y) (chr v)
        move
      Get -> do
        y <- popStackSafe
        x <- popStackSafe
        instr <- getInstructionAt (x, y)
        pushStack (ord instr)
        move
      NoOp -> move
      End -> return ()

runInstrs :: State VM ()
runInstrs = do
  instr <- getInstruction
  unless (instr == End) $ runInstr instr >> runInstrs

interpret :: R.StdGen -> String -> String
interpret stdGen str = let
  instrs = V.fromList $ map UV.fromList (lines str)
  vm = VM { instrs = instrs
          , stack = []
          , instrptr = (0, 0)
          , currDirection = R
          , outputBuf = T.empty
          , stdGen = stdGen
          }
  in
    T.unpack $ outputBuf $ execState runInstrs vm
