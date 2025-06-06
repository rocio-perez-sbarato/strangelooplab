module HypersetGraph.Decorator where

import Data.Array
import Data.Typeable (cast, Typeable)
import HypersetGraph.Types
import HypersetGraph.Operations

computeDecorations :: LabGraph String -> Array Vertex (HFS String)
computeDecorations (LabGraph gr label) = listArray (bounds gr) [decorate [] v | v <- indices gr]
  where
    decorate visited v
      | v `elem` visited = S []  -- Evitar ciclos
      | null children  = label v     -- Caso base: sin hijos
      | otherwise = unionHFS (label v) (S nonEmptyChildDecs) -- Caso recursivo: con hijos
      where
        children = gr ! v
        visited' = v : visited
        nonEmptyChildDecs = filter (not . isEmpty) 
                              (map (decorate visited') children)
