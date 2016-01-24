module EscapeTheMines where

import           Data.Maybe
import qualified Data.Set   as S

type XY = (Int,Int)
data Move = U | D | R | L  deriving (Eq, Show)

diff :: XY -> Move -> XY
diff (x, y) move = case move of
  U -> (x, y-1)
  D -> (x, y+1)
  R -> (x+1, y)
  L -> (x-1, y)

solve :: [[Bool]] -> XY -> XY -> [Move]
solve grid miner exit = fromJust $ solve_ grid miner exit S.empty

solve_ :: [[Bool]] -> XY -> XY -> S.Set XY -> Maybe [Move]
solve_ grid pos exit walked
  | pos == exit = Just []
  | otherwise = do
    let moves = getOpenMoves grid pos walked
    case moves of
      [] -> Nothing
      _ -> let possibilities =
                 filter (/= Nothing) $ map (
                 \move -> do
                   moves <- solve_ grid (diff pos move) exit (S.insert pos walked)
                   return $ move:moves
                 ) moves
           in if possibilities == [] then Nothing else head possibilities
  where
    getBrick :: [[Bool]] -> XY -> Bool
    getBrick grid (x, y)
      | x < 0 || x >= length grid = False
      | y < 0 || y >= length (grid !! 0) = False
      | otherwise = grid !! x !! y
    getOpenMoves :: [[Bool]] -> XY -> S.Set XY -> [Move]
    getOpenMoves grid pos@(posx, posy) walked =
      [d|d <- [U, D, R, L], let p = diff pos d, S.notMember p walked, getBrick grid p]

unmap = map (map (== ' '))
