module ExplosiveSum where

explSum :: Int -> [[Int]]
explSum n = concatMap (_explSum n) [1..n]

_explSum :: Int -> Int -> [[Int]]
_explSum m n
  | m == n = [[m]]
  | otherwise = map (n:) $ concatMap (_explSum (m-n)) [n..m-n]
