module HypersetGraph.GraphBuilder where

import Data.Array
import HypersetGraph.Types

-- === Construcción del grafo ===
-- Construye un Graph a partir de los límites (bounds) de los vértices y una lista de aristas (edges)
buildG :: Bounds -> [Edge] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds [(v, w) | (v, w) <- edges]