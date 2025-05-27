module HypersetGraph.DotExport where

import Data.Array
import Data.Typeable (Typeable)
import HypersetGraph.Types

-- DOT export 
showGraphViz :: (Show a, Typeable a) => LabGraph a -> String
showGraphViz (LabGraph gr label) =
  "digraph G {\n" ++
  concatMap showEdges (assocs gr) ++
  "}\n"
  where
    showEdges (v, ws) =
      concat [ "  " ++ show v ++ " -> " ++ show w ++ ";\n" | w <- ws ]