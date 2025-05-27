module HypersetGraph.Decorator where

import Data.Array
import Data.Typeable (cast, Typeable)
import HypersetGraph.Types

-- === Decoración ===
decorateAll :: LabGraph String -> Array Vertex (HFS String)
decorateAll (LabGraph gr label) = decs
  where
    decs = listArray (bounds gr) [decorate [] v | v <- indices gr]
    decorate visited v
      | v `elem` visited = U (label v ++ " = {" ++ label v ++ "}") -- marca del ciclo 
      | otherwise =
          let children = gr ! v
              childDecs = map (decorate (v : visited)) children
          in S (childDecs ++ [U (label v)]) -- Si no tiene hijos, solo queda el label