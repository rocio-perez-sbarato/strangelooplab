module Viz where

import Elements ( Edge, Graph, LabGraph(..) ) 
import Example ( genInclosure )
import Data.Set (toList)
import System.IO ()
import Data.Array ( (!), indices ) 

-- Generar archivo DOT para Graphviz
showGraphViz :: LabGraph String String -> String
showGraphViz (LabGraph gr lab)  = 
    "digraph name {\n" ++
    "rankdir=LR;\n" ++
    concatMap showNode (indices gr) ++
    concat (zipWith showEdge [1..] (edges gr)) ++
    "}\n"
  where
    showEdge i (from, t, to)
      | i == 2 && from == 0 && to == 0 =
          show from ++ " -> " ++ show to ++
          " [label=\"" ++ t ++ "\", color=red];\n"
      | otherwise =
          show from ++ " -> " ++ show to ++ " [label=\"" ++ t ++ "\"];\n"

    showNode v =
      show v ++ " [label=\"" ++ lab v ++ "\"];\n"

edges :: Graph e -> [Edge e]
edges g = [ (v, l, w) | v <- indices g, (l, w) <- g!v ]