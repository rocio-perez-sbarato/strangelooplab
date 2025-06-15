module HypersetGraph.Operations where

import HypersetGraph.Types
import qualified Data.Set as Set

-- Al fin y al cabo, los conjuntos HFS son listas
-- Caso U: ponemos los U x en una lista para meterlos en un conjunto
-- Caso S: concatenamos las listas de los conjuntos
unionHFS :: Eq t => HFS t -> HFS t -> HFS t
unionHFS (S xs) (S ys) = S (unionList xs ys)
unionHFS (S xs) y      = S (unionList xs [y]) 
unionHFS x      (S ys) = S (unionList [x] ys)
unionHFS x      y      = S (unionList [x] [y]) 

-- Unión de listas sin repetir elementos 
unionList :: Eq t => [HFS t] -> [HFS t] -> [HFS t]
unionList xs ys = xs ++ [ y | y <- ys, y `notElem` xs ]

-- Definición de conjunto vacío como S []
isEmpty :: HFS t -> Bool
isEmpty (S []) = True
isEmpty _      = False

-- Cuenta la cantidad de vértices en un RefHFS
countRefHFS :: RefHFS t -> Int
countRefHFS (RefU (_, _, _)) = 1
countRefHFS (RefS _ _ children) = 1 + sum (map countRefHFS children)

-- Recorre el RefHFS y junta (Vertex, Label) 
-- Asocia cada vértice único con su label (primera aparición)
collectLabels :: RefHFS t -> [(Vertex, Label)]
collectLabels refhfs = go refhfs Set.empty
  where
    go (RefU (_, label, v)) seen
      | v `Set.member` seen = []
      | otherwise           = [(v, label)]
    go (RefS label v children) seen
      | v `Set.member` seen = []
      | otherwise           = (v, label) : concatMap (\c -> go c (Set.insert v seen)) children

-- Cuenta los vértices únicos en el RefHFS
-- Contempla ciclos y referencias entre vértices
-- Evita repeticiones usando un Set
countVertices :: RefHFS t -> Int
countVertices refhfs = Set.size (collectVertices refhfs Set.empty)
  where
    collectVertices (RefU (_, _, v)) seen
      | v `Set.member` seen = seen
      | otherwise           = Set.insert v seen
    collectVertices (RefS _ v children) seen
      | v `Set.member` seen = seen
      | otherwise           = foldr collectVertices (Set.insert v seen) children