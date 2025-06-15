module HypersetGraph.SetToGraph where 

import HypersetGraph.Types 
import HypersetGraph.Operations 

import Data.Array

-- Construye el LabGraph
setToLabGraph :: RefHFS t -> LabGraph Label
setToLabGraph refhfs =
  let g = setToGraph refhfs
      labelsArr = getLabels refhfs
      labeling v = S [U (labelsArr ! v)]   
  in LabGraph g labeling

-- Convierte un conjunto HFS a un grafo
setToGraph :: RefHFS t -> Graph
setToGraph refhfs =
  let n = countVertices refhfs
  in listArray (0, n - 1) [getNeighbours refhfs v | v <- [0..n-1]]

-- Obtiene los vecinos de un vértice en un RefHFS
getNeighbours :: RefHFS t -> Vertex -> [Vertex]
getNeighbours (RefU (_, _, v)) target = []
getNeighbours (RefS _ v children) target
  | v == target = map getVertex children
  | otherwise   = concatMap (\child -> getNeighbours child target) children
  where
    getVertex (RefU (_, _, v)) = v
    getVertex (RefS _ v _)     = v

-- Extrae los labels de cada vértice en un array indexado por Vertex
getLabels :: RefHFS t -> Array Vertex Label
getLabels refhfs =
  let pairs = collectLabels refhfs
      n = countVertices refhfs
  in array (0, n - 1) pairs