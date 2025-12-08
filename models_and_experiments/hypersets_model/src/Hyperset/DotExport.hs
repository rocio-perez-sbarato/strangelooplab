{-|
Module      : Hyperset.Types
Description : Exportación de Graph y LabGraph a archivo .dot
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.DotExport where
import Data.Array ( assocs, indices )
import Data.Typeable (Typeable)
import Hyperset.Types ( LabGraph(..) )
import Hyperset.Pretty ( prettyHFS )

-- | DOT export 
showGraphViz :: (Show a, Typeable a) => LabGraph a -> String
showGraphViz (LabGraph gr label) =
  "digraph G {\n" ++
  concatMap showEdges (assocs gr) ++
  "}\n"
  where
    showEdges (v, ws) =
      concat [ "  " ++ show v ++ " -> " ++ show w ++ ";\n" | w <- ws ]

-- | DOT export con label 
showLabGraphViz :: (Show a, Typeable a) => LabGraph a -> String
showLabGraphViz (LabGraph gr label) =
  "digraph G {\n" ++
  concatMap showNode (indices gr) ++
  concatMap showEdges (assocs gr) ++
  "}\n"
  where
    showNode v =
      "  " ++ show v ++ " [label=" ++ show (prettyHFS (label v)) ++ "];\n"
    showEdges (v, ws) =
      concat [ "  " ++ show v ++ " -> " ++ show w ++ ";\n" | w <- ws ]