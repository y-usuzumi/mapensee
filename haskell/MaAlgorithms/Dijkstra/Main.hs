module Main where

import Data.Graph
       (
         Graph,
         Vertex,
         components,
         graphFromEdges,
         reachable,
         topSort,
         vertices
       )
import qualified Data.Map.Strict as M

sToMap :: String -> M.Map Char [Char]
sToMap [] = M.empty
sToMap (x:[]) = M.singleton x []
sToMap (x:x':xs) = M.unionWith (++) (M.singleton x [x']) (sToMap (x':xs))

toMap :: [String] -> M.Map Char [Char]
toMap = M.unionsWith (++) . fmap sToMap

mapToGraph :: M.Map Char [Char] -> (Graph, Vertex -> (Char, Char, [Char]), Char -> Maybe Vertex)
mapToGraph = graphFromEdges . map (\(k, ch) -> (k, k, ch)) . M.assocs

toGraph :: [String] -> (Graph, Vertex -> (Char, Char, [Char]), Char -> Maybe Vertex)
toGraph = mapToGraph . toMap

test :: [String]
test =
  [ "wte"
  , "aev"
  , "hat"
  , "whr"
  , "tvr"
  ]

first :: (a, b, c) -> a
first (x, _, _) = x

main :: IO ()
main = do
  let (g, vertexToNode, keyToVertex) = toGraph test
  let vs = vertices g
  print $ map vertexToNode vs
  print $ map (first . vertexToNode) $ topSort g
  print $ components g
