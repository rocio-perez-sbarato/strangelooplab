{-|
Module      : Hyperset.Types
Description : Tipos principales para representar sistemas de ecuaciones en ZFA
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}


module Hyperset.GraphBuilder where
import Data.Array
import Hyperset.Types

-- === Construcción del grafo ===
-- | Construye un Graph a partir de los límites (bounds) de los vértices y una lista de aristas (edges)
buildG :: Bounds -> [Edge] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds [(v, w) | (v, w) <- edges]