{-|
Module      : Hyperset.Types
Description : Tipos principales para representar sistemas de ecuaciones en ZFA
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}


module Hyperset.Decorator where
import Data.Array
import Hyperset.Types
import Hyperset.Operations

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
