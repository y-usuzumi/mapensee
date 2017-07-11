{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module Graph where

class Graph g where
  type Vertex g
  empty :: g
  fromVertex :: Vertex g -> g


data ConcreteGraph node where
  ConcreteGraph :: [node] -> [(node, node)] -> ConcreteGraph node


instance Graph (ConcreteGraph a) where
  type Vertex (ConcreteGraph a) = a
  empty = ConcreteGraph [] []
  fromVertex g = ConcreteGraph [g] []

