module HypersetGraph.Decorator where

import Data.Array
import Data.Typeable (cast, Typeable)
import HypersetGraph.Types

-- === Decoración ===
computeDecorations :: LabGraph String -> Array Vertex (HFS String)
computeDecorations (LabGraph gr label) = decs
  where
    decs = listArray (bounds gr) [decorate [] v | v <- indices gr]

    decorate visited v
      | v `elem` visited = S [] -- skip hecho a mano
      | otherwise =
          let children = gr ! v
              childDecs = map (decorate (v : visited)) children
              -- por si hay un ciclo en uno de los hijos de v
              nonEmptyChildren = filter (not . isEmpty) childDecs 
          in S (nonEmptyChildren ++ [U (label v)])

    isEmpty (S []) = True
    isEmpty _      = False

