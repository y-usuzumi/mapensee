{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

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
getInstr vm@VM{..} = instrs ^? ix instrPtr
  where
    instrs = vmInstrs
    instrPtr = vmInstrPtr

getData :: VM -> Int
getData vm@VM{..} = fromMaybe 0 (mem ^? ix memPtr)
  where
    mem = vmMem
    memPtr = vmMemPtr

setData :: VM -> (Int -> Int) -> VM
setData vm@VM{..} f = let
  mem = compensateMemory vmMem (vmMemPtr + 1)
  newMem = take vmMemPtr mem ++ [raw $ unraw $ f (mem !! vmMemPtr)] ++ drop (vmMemPtr + 1) mem
  in
    vm {vmMem = newMem}

nextInstr :: VM -> VM
nextInstr vm = let
  instrs = vmInstrs vm
  instrPtr = vmInstrPtr vm
  in
    case getInstr vm of
    Just JMPZ -> vm {vmInstrPtr = if getData vm == 0 then jmp instrs instrPtr else instrPtr+1}
    Just JMPNZ -> vm {vmInstrPtr = if getData vm /= 0 then jmp instrs instrPtr else instrPtr-1}
    _ -> vm {vmInstrPtr=instrPtr+1}

runVM :: VM -> Maybe String
runVM vm@VM{..} = let
  instr = getInstr vm
  newVM = nextInstr vm
  in
  case getInstr vm of
    Just PINC -> runVM (newVM {vmMemPtr=vmMemPtr+1})
    Just PDEC -> runVM (newVM {vmMemPtr=vmMemPtr-1})
    Just INCR -> runVM $ setData newVM (+1)
    Just DECR -> runVM $ setData newVM (subtract 1)
    Just READ -> let
      (newIn, newMem) = accept vmIn vmMemPtr vmMem
      in
        runVM (vm {vmIn=newIn, vmMem=newMem})
    Just SPIT -> let
      newOut = spit vmOut vmMemPtr vmMem
      in
        runVM (vm {vmOut=newOut})
    Just JMPZ -> runVM newVM
    Just JMPNZ -> runVM newVM
    Nothing -> Just vmOut
