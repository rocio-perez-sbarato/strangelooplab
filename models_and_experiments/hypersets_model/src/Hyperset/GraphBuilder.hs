{-|
Module      : Hyperset.Types
Description : Construcción de grafo tal como en Data.Graph
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.GraphBuilder where
import Data.Array ( accumArray )
import Hyperset.Types ( Bounds, Edge, Graph )

-- === Construcción del grafo ===
-- | Construye un Graph a partir de los límites (bounds) de los vértices y una lista de aristas (edges)
buildG :: Bounds -> [Edge] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds [(v, w) | (v, w) <- edges]