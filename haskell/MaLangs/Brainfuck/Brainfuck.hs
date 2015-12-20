{-# LANGUAGE DeriveAnyClass #-}

module Brainfuck
    ( executeString
    ) where

import Control.Lens
import Data.List.Lens
import Data.Maybe

type Input = String
type Output = String
type Memory = [Int]
type Pointer = Int

data VM = VM
          { vmIn :: Input
          , vmOut :: Output
          , vmMem :: Memory
          , vmMemPtr :: Int
          , vmInstrs :: [Instruction]
          , vmInstrPtr :: Int
          }

data Instruction = PINC   -- >
                 | PDEC   -- <
                 | INCR   -- +
                 | DECR   -- -
                 | SPIT   -- .
                 | READ   -- ,
                 | JMPZ   -- [
                 | JMPNZ  -- ]
                 deriving (Ord, Eq, Show)

data Token = Token Instruction
           | JmpToken [Token] deriving (Show)

jmp :: [Instruction] -> Pointer -> Pointer
jmp ins p = case ins !! p of
  JMPZ -> jmpForward ins (p+1)
  JMPNZ -> jmpBackward ins (p-1)
  _ -> p
  where
    jmpForward :: [Instruction] -> Pointer -> Pointer
    jmpForward ins p
      | (ins !! p) == JMPNZ = p
      | (ins !! p) == JMPZ = jmpForward ins (jmp ins p + 1)
      | otherwise = jmpForward ins (p+1)

    jmpBackward :: [Instruction] -> Pointer -> Pointer
    jmpBackward ins p
      | (ins !! p) == JMPZ = p
      | (ins !! p) == JMPNZ = jmpBackward ins (jmp ins p - 1)
      | otherwise = jmpBackward ins (p-1)

tokenize :: String -> [Instruction]
tokenize = map tokenizeSingle
  where
    tokenizeSingle :: Char -> Instruction
    tokenizeSingle i = case i of
      '>' -> PINC
      '<' -> PDEC
      '+' -> INCR
      '-' -> DECR
      '.' -> SPIT
      ',' -> READ
      '[' -> JMPZ
      ']' -> JMPNZ
      _ -> error "Invalid instructions"

raw :: Char -> Int
raw = fromEnum

unraw :: Int -> Char
unraw = toEnum

compensateMemory :: Memory -> Int -> Memory
compensateMemory m minLength = let
  l = length m
  in
    if minLength > l then m ++ replicate 0 (minLength - l) else m

accept :: Input -> Pointer -> Memory -> (Input, Memory)
accept [] _ _ = error "Nothing to read"
accept (x:xs) pointer memory = let
  newMemory = compensateMemory memory (pointer + 1)
  in
  (xs, take pointer newMemory ++ [raw x] ++ drop (pointer + 1) newMemory)

spit :: Output -> Pointer -> Memory -> Output
spit output pointer memory = let
  newMemory = compensateMemory memory (pointer + 1)
  in
  output ++ [unraw $ newMemory !! pointer]

-- | Interprets the Brainfuck source code from the first argument, while
-- supplying it with input from the second. May fail on insufficient input.
executeString :: String -> String -> Maybe String
executeString code input = let
  vm = VM { vmIn=input
          , vmOut=""
          , vmMem=[]
          , vmMemPtr=0
          , vmInstrs=tokenize code
          , vmInstrPtr=0
          }
  in
    runVM vm

getInstr :: VM -> Maybe Instruction
getInstr vm = instrs ^? ix instrPtr
  where
    instrs = vmInstrs vm
    instrPtr = vmInstrPtr vm

getData :: VM -> Int
getData vm = fromMaybe 0 (mem ^? ix memPtr)
  where
    mem = vmMem vm
    memPtr = vmMemPtr vm

nextInstr :: VM -> VM
nextInstr vm = case getInstr vm of
  Just JMPZ -> undefined
  Just JMPNZ -> undefined
  _ -> vm {vmInstrPtr=vmInstrPtr vm+1}

runVM :: VM -> Maybe String
runVM vm = case getInstr vm of
    Just PINC -> runVM (vm {vmMemPtr=vmMemPtr vm+1})
    Just PDEC -> runVM (vm {vmMemPtr=vmMemPtr vm-1})
