{-|
Module      : Hyperset.Types
Description : Módulo que realiza el pasaje del tipo de datos HFS al tipo de datos LabGraph,
              respetando la herencia propuesta por HFS
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.SetToGraph where 
import Hyperset.Types
    ( LabGraph(..), Labeling, Graph, RefHFS(..), ID, Label ) 
import Hyperset.Operations ( countVertices ) 
import Data.Array ( listArray )
import Data.List (nub)

-- | Construye el LabGraph, recibiendo el labeling como parámetro
setToLabGraph :: RefHFS t -> Labeling Label -> LabGraph Label
setToLabGraph refhfs labeling =
  let g = setToGraph refhfs
  in LabGraph g labeling

-- | Convierte un conjunto HFS a un grafo
setToGraph :: RefHFS t -> Graph
setToGraph refhfs =
  let n = countVertices refhfs
  in listArray (0, n - 1) [getChildren refhfs v | v <- [0..n-1]]

-- | Obtiene los IDs de los hijos de un vértice en un RefHFS
getChildren :: RefHFS t -> ID -> [ID]
getChildren (RefU (_, v)) target = []
getChildren (RefS v children) target
  | v == target = nub $ map getVertex children
  | otherwise   = nub $ concatMap (\child -> getChildren child target) children
  where
    getVertex (RefU (_, v)) = v
    getVertex (RefS v _)     = v