module HypersetGraph.Decorator where

import Data.Array
import Data.Typeable (cast, Typeable)
import HypersetGraph.Types
import HypersetGraph.Operations

-- === Decoración ===
computeDecorations :: LabGraph String -> Array Vertex (HFS String)
computeDecorations (LabGraph gr label) = decs
  where
    decs = listArray (bounds gr) [decorate [] v | v <- indices gr]

    decorate visited v
      | v `elem` visited = S [] -- skip para evitar ciclos
      | otherwise =
          let children = gr ! v
              childDecs = map (decorate (v : visited)) children
              nonEmptyChildren = filter (not . isEmpty) childDecs
          in unionHFS (S nonEmptyChildren) (label v)