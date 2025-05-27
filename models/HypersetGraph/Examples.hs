module HypersetGraph.Examples where

import Data.Array
import HypersetGraph.Types
import HypersetGraph.GraphBuilder

-- === Ejemplos ===

-- Ejemplo 1
edges1 :: [Edge]
edges1 = [ (0, 1), (0, 2), (1, 2) ]
labels1 :: Array Vertex String
labels1 = listArray (0, 2) ["x", "0", "x,y"]
labGraph1 :: LabGraph String
labGraph1 = LabGraph (buildG (bounds labels1) edges1) (labels1 !)

-- Ejemplo 2
edges2 :: [Edge]
edges2 = [ (0, 1), (0, 2), (1, 3), (1, 4), (1, 2), (2, 3), (2, 1) ]
labels2 :: Array Vertex String
labels2 = listArray (0, 4) ["c", "a", "b", "maxi", "claire"]
labGraph2 :: LabGraph String
labGraph2 = LabGraph (buildG (bounds labels2) edges2) (labels2 !)

-- Ejemplo 3 (omega)
edges3 :: [Edge]
edges3 = [ (0, 0) ]
labels3 :: Array Vertex String
labels3 = listArray (0, 0) ["X"]
labGraph3 :: LabGraph String
labGraph3 = LabGraph (buildG (bounds labels3) edges3) (labels3 !)

-- Lista de ejemplos
type NamedExample = (String, LabGraph String)
examples :: [NamedExample]
examples =
  [ ("example1", labGraph1)
  , ("example2", labGraph2)
  , ("example3_cycle", labGraph3)
  ]
