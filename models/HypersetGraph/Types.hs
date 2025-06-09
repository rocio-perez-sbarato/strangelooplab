module HypersetGraph.Types where

import Data.Array

-- === Tipo HereditaryFiniteSets ===
-- Usa una lista para almacenar sus elementos
data HFS t = S [HFS t] | U t deriving (Eq, Show)

data NumberedHFS t = NumberedS Vertex [NumberedHFS t] | NumberedU (t, Vertex)
  deriving (Show)

-- === Tipos de grafos ===
type Vertex = Int
type Graph = Array Vertex [Vertex]
type Edge = (Vertex, Vertex)
type Bounds = (Vertex, Vertex)

type Labeling a = Vertex -> HFS a
data LabGraph n = LabGraph Graph (Labeling n) 