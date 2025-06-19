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
  in listArray (0, n - 1) [getChildren refhfs v | v <- [0..n-1]]

-- Obtiene los hijos de un vértice en un RefHFS
getChildren :: RefHFS t -> ID -> [ID]
getChildren (RefU (_, _, v)) target = []
getChildren (RefS _ v children) target
  | v == target = map getVertex children
  | otherwise   = concatMap (\child -> getChildren child target) children
  where
    getVertex (RefU (_, _, v)) = v
    getVertex (RefS _ v _)     = v 

-- Extrae los labels de cada vértice en un array indexado por ID
getLabels :: RefHFS t -> Array ID Label
getLabels refhfs =
  let pairs = collectLabels refhfs
      n = countVertices refhfs
  in array (0, n - 1) pairs