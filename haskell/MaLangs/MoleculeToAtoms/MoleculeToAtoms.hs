module MoleculeToAtoms where

import Data.Map as M
import Debug.Trace
import Control.Applicative

data Token = Name String | Number Int | LB | RB deriving (Ord, Eq, Show)
type Result = Map String Int

tokenize :: String -> [Token]
tokenize s = let
  t = tokenizeName s
      <|> tokenizeNumber s
      <|> tokenizeLB s
      <|> tokenizeRB s
  in
  case t of
    Just (t, xs) -> t:tokenize xs
    Nothing -> []

type TokenizeFunc = String -> Maybe (Token, String)

tokenizeName :: TokenizeFunc
tokenizeName [] = Nothing
tokenizeName (x:y:xs)
  | x `elem` ['A'..'Z'] && y `elem` ['a'..'z'] = Just (Name (x:y:[]), xs)
tokenizeName (x:xs)
  | x `elem` ['A'..'Z'] = Just (Name [x], xs)
tokenizeName _ = Nothing

tokenizeNumber :: TokenizeFunc
tokenizeNumber xs = let
  num = takeWhile (`elem` ['0'..'9']) xs
  in
  if num == "" then Nothing else Just (Number (read num), drop (length num) xs)

tokenizeLB :: TokenizeFunc
tokenizeLB "" = Nothing
tokenizeLB (x:xs)
  | x `elem` "[(" = Just (LB, xs)
  | otherwise = Nothing

tokenizeRB :: TokenizeFunc
tokenizeRB "" = Nothing
tokenizeRB (x:xs)
  | x `elem` ")]" = Just (RB, xs)
  | otherwise = Nothing

merge :: Result -> Result -> Result
merge = M.unionWith (+)

parse :: [Token] -> (Result, [Token])
parse [] = (M.empty, [])
parse (Name name:Number number:ts) = let
  (result, remaining) = parse ts
  in
    (merge (M.singleton name number) result, remaining)
parse (Name name:ts) = let
  (result, remaining) = parse ts
  in
    (merge (M.singleton name 1) result, remaining)
parse (LB:ts) = let
  (result, remaining) = parse ts
  in
    case remaining of
      (Number number:tss) -> let
        (result', tss') = parse tss
        in
          (merge (M.map (*number) result) result', tss')
      _ -> (result, remaining)
parse (RB:ts) = (M.empty, ts)

parseMolecule :: String -> Either String [(String,Int)]
parseMolecule formula = let
  (result, tokens) = parse $ tokenize formula
  in
    -- Left $ show $ tokenize formula
    trace (show tokens) $ Right (M.toList result)
