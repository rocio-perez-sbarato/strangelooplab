{-|
Module      : Hyperset.Types
Description : Tipos principales para representar sistemas de ecuaciones en ZFA
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}


module Hyperset.SetToGraph where 
import Hyperset.Types 
import Hyperset.Operations 
import Data.Array

-- | Construye el LabGraph, recibiendo el labeling a mano
setToLabGraph :: RefHFS t -> Labeling Label -> LabGraph Label
setToLabGraph refhfs labeling =
  let g = setToGraph refhfs
  in LabGraph g labeling

-- | Convierte un conjunto HFS a un grafo
setToGraph :: RefHFS t -> Graph
setToGraph refhfs =
  let n = countVertices refhfs
  in listArray (0, n - 1) [getChildren refhfs v | v <- [0..n-1]]

-- | Obtiene los hijos de un vértice en un RefHFS
getChildren :: RefHFS t -> ID -> [ID]
getChildren (RefU (_, v)) target = []
getChildren (RefS v children) target
  | v == target = map getVertex children
  | otherwise   = concatMap (\child -> getChildren child target) children
  where
    getVertex (RefU (_, v)) = v
    getVertex (RefS v _)     = v 