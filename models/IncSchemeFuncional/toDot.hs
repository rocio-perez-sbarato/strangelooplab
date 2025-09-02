module IncSchemeFuncional.ToDot where 

import IncSchemeFuncional.Types
import IncSchemeFuncional.DenoteSystem 
import IncSchemeFuncional.SetsAndPredicates 
import Data.List 
import Data.Array 
import Data.Typeable 

-- Anda mal
-- Para RefHFS bonito
prettyHFS :: Show t => RefHFScheme t -> String
prettyHFS (RefU (_, v))     = show v
prettyHFS (RefS v children) =
    show v ++ "{" ++ intercalate ", " (map prettyHFS children) ++ "}"
prettyHFS (Apply lst) =
    "Apply[" ++ intercalate ", " [show lbl ++ "->" ++ show to | (lbl, to, _) <- lst] ++ "]"

-- DOT export para LabGraph con Maybe Label en aristas
showLabGraphViz :: (Show n, Show e) => LabGraph n e -> String
showLabGraphViz (LabGraph gr label) =
  "digraph G {\n" ++
  concatMap showNode (indices gr) ++
  concatMap showEdges (assocs gr) ++
  "}\n"
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
