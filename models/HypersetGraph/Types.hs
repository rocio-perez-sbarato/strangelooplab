module HypersetGraph.Types where

import Data.Array

-- === Tipo HFS ===
data HFS t = S [HFS t] | U t deriving (Eq, Show)

-- === Tipos de grafos ===
type Vertex = Int
type Graph = Array Vertex [Vertex]
type Edge = (Vertex, Vertex)
type Bounds = (Vertex, Vertex)

type Labeling a = Vertex -> a
data LabGraph n = LabGraph Graph (Labeling n) 