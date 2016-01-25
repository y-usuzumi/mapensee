{-# LANGUAGE PatternSynonyms #-}

module Befunge93 where

import Control.Monad.State
import System.Random (StdGen)

pattern Add = '+'
pattern Sub = '-'
pattern Mul = '*'
pattern Div = '/'
pattern Mod = '%'

type Instr = Char
type Instrs = [[Instr]]
type Stack = [Int]
data NoType

pushStack :: Int -> State Stack ()
pushStack i = state $ \xs -> ((), i:xs)

popStack :: State Stack Int
popStack = state $ \(x:xs) -> (x, xs)

runInstr :: Instr -> NoType
runInstr Add = undefined

interpret :: StdGen -> String -> String
interpret = undefined
