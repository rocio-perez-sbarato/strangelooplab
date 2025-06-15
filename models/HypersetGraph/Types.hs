module HypersetGraph.Types where

import Data.Array

-- === Tipo HereditaryFiniteSets ===
-- Usa una lista para almacenar sus elementos
data HFS t = S [HFS t] | U t 
  deriving (Eq, Show)

-- === Tipo HereditaryFiniteSets con manejo de ciclos ===
-- Usa una lista para almacenar sus elementos y usa labels y vértices para manejar ciclos
-- usa una etiqueta y un vértice para identificar cada elemento

data RefHFS t = RefS Label Vertex [RefHFS t] | RefU (t, Label, Vertex)
  deriving (Show)

-- === Tipos de grafos ===
type Label = String
type Vertex = Int
type Graph = Array Vertex [Vertex]
type Edge = (Vertex, Vertex)
type Bounds = (Vertex, Vertex)

type Labeling a = Vertex -> HFS a 
data LabGraph n = LabGraph Graph (Labeling n) 