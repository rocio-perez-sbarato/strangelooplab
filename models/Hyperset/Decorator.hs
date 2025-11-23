{-|
Module      : Hyperset.Types
Description : Decorado de grafos etiquetados siguiendo la definición formal
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.Decorator where
import Data.Array ( Array, (!), bounds, indices, listArray )
import Hyperset.Types
      ( Graph, HFS(S), LabGraph(..), Labeling, Vertex )
import Hyperset.Operations ( unionHFS )

-- | Decorado de cada vértice del grafo
computeDecorations :: LabGraph String -> Array Vertex (HFS String)
computeDecorations (LabGraph gr label) = listArray (bounds gr) 
                                          [decorate gr label [] v | v <- indices gr]

-- | Decorado de un vértice 
decorate :: Graph -> Labeling String -> [Vertex] -> Vertex -> HFS String
decorate gr label visited v
      | v `elem` visited = label v -- Agrega el label aunque esté visitado
      | null children  = label v
      | otherwise = unionHFS (label v) (S childDecs)
      where
        children = gr ! v
        visited' = v : visited
        childDecs = map (decorate gr label visited') children
