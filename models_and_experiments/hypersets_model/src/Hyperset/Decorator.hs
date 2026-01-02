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
import Hyperset.Operations ( unionHFS )
import Hyperset.Pretty (prettyHFS, prettyVertex)

-- | Decorado de cada vértice del grafo
computeDecorations :: LabGraph String -> Array Vertex (HFS String)
computeDecorations (LabGraph gr label) =
  listArray (bounds gr)
    [ decorate gr label [] v | v <- indices gr ]

decorate :: Graph -> Labeling String -> [Vertex] -> Vertex -> HFS String
decorate gr label visited v
  | v `elem` visited = U ("d(" ++ prettyVertex(label v) ++ ")")
  | null children    = label v
  | otherwise        = unionHFS (label v) (S childDecs)
  where
    children  = gr ! v
    visited'  = v : visited
    childDecs = map (decorate gr label visited') children

-- | Simplificación del decorado de cada vértice del grafo
computeDecorationsShort :: LabGraph String -> Array Vertex (HFS String)
computeDecorationsShort (LabGraph gr label) = listArray (bounds gr) 
                                          [decorateShort gr label [] v | v <- indices gr]

-- | Decorado simplificado de un vértice 
decorateShort :: Graph -> Labeling String -> [Vertex] -> Vertex -> HFS String
decorateShort gr label visited v
  | v `elem` visited = U ("d(" ++ prettyVertex(label v) ++ ")")
  | null children    = label v
  | otherwise =
      unionHFS (label v) (S (map d children))
  where
    children = gr ! v
    d x = U ("d(" ++ prettyVertex(label x) ++ ")")
