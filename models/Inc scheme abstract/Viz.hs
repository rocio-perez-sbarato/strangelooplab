module Viz where

import Elements ( Edge, Graph, LabGraph(..) ) 
import Graph 
import Data.Set (toList)
import Data.Array ( (!), indices ) 
import System.IO
    ( utf8, hSetEncoding, hPutStr, withFile, IOMode(WriteMode) )

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

-- | Codificación en UTF-8 para que símbolos Unicode y su corrrecta visualización
saveGraphViz :: FilePath -> LabGraph String String -> IO ()
saveGraphViz path graph =
    withFile path WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h (showGraphViz graph)