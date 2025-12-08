module IncSchemeFuncional.SetsAndPredicates where

import IncSchemeFuncional.Types
    ( Inclosure(Inclosure),
      LabGraph(LabGraph),
      RefHFScheme(..),
      System,
      Vertex,
      Label,
      Graph ) 
import IncSchemeFuncional.DenoteSystem ( denoteSystem )
import Data.Array ( assocs, bounds, listArray )
import Data.List ( nub ) 
import IncSchemeFuncional.Schemes

-- Devuelve todos los vértices
allVertices :: RefHFScheme t -> [Vertex]
allVertices (RefU (_, v))     = [v]
allVertices (RefS v children) = v : concatMap allVertices children
allVertices (Apply lst)       = [to | (_, to, _) <- lst]  -- solo destinos

countVertices :: RefHFScheme t -> Int
countVertices refhfs = length . nub $ allVertices refhfs

-- Obtener nodo por vértice
getVertexById :: RefHFScheme t -> Vertex -> RefHFScheme t
getVertexById node@(RefU (_, v)) target
  | v == target = node
  | otherwise   = error $ "Vértice no encontrado: " ++ show target
getVertexById node@(RefS v children) target
  | v == target = node
  | otherwise   = searchChildren children
  where
    searchChildren [] = error $ "Vértice no encontrado: " ++ show target
    searchChildren (c:cs) =
      if target `elem` allVertices c then getVertexById c target else searchChildren cs
getVertexById node@(Apply lst) target
  | any (\(_, to, _) -> to == target) lst = node
  | otherwise = error $ "Vértice no encontrado: " ++ show target

-- Devuelve aristas de un vértice
getChildren :: RefHFScheme Label -> Vertex -> [(Maybe Label, Vertex)]
getChildren (RefU (_, _)) _ = []

getChildren (RefS v children) target
  | v == target = nub $ concatMap childEdges children
  | otherwise   = concatMap (\c -> if isApply c then [] else getChildren c target) children
  where
    childEdges :: RefHFScheme Label -> [(Maybe Label, Vertex)]
    childEdges (RefU (_, v')) = [(Nothing, v')]
    childEdges (RefS v' _)    = [(Nothing, v')]
    childEdges (Apply lst)    = [(Just lbl, to) | (lbl, to, _) <- lst]

    isApply (Apply _) = True
    isApply _         = False

getChildren (Apply lst) _ = [(Just lbl, to) | (lbl, to, _) <- lst]

-- Construir grafo
setToGraph :: RefHFScheme Label -> Graph Label
setToGraph refhfs =
  let n = countVertices refhfs
  in listArray (0, n - 1) [getChildren refhfs v | v <- [0..n-1]]

-- Construir LabGraph
setToLabGraph :: RefHFScheme Label -> LabGraph (RefHFScheme Label) Label
setToLabGraph refhfs =
  let g = setToGraph refhfs
  in LabGraph g (\v -> getVertexById refhfs v)
