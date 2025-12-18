module HypersetPredicate.ToDot where 
import HypersetPredicate.Types
import Data.Array 

-- | DOT export para LabGraph con Maybe Label en aristas
showLabGraphViz :: (Show n, Show e) => LabGraph n e -> String
showLabGraphViz (LabGraph gr label) =
  "digraph G {\n"
  ++ "  layout=neato;\n"
  ++ "  overlap=prism;\n"
  ++ "  splines=curved;\n"
  ++ "  sep=1.2;\n"
  ++ "  node [shape=circle];\n\n"
  ++ concatMap showNode (indices gr)
  ++ concatMap showEdges (assocs gr)
  ++ "}\n"
  where
    showNode v =
      "  " ++ show v ++ " [label=" ++ show (label v) ++ "];\n"

    showEdges (from, ws) =
      concat [ showEdge from lbl to | (lbl, to) <- ws ]

    showEdge from mlabel to =
      "  " ++ show from ++ " -> " ++ show to ++ labelStr ++ ";\n"
      where
        labelStr = case mlabel of
                      Nothing -> ""
                      Just s  -> " [label=" ++ show s ++ "]"