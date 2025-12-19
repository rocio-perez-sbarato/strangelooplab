module HypersetIncScheme.SetToGraph where

import HypersetIncScheme.Types
import Data.Array ( assocs, bounds, listArray )
import Data.List ( nub ) 

-- Devuelve todos los vértices
allVertices :: RefHFScheme t -> [Vertex]
allVertices (RefU (_, v))     = [v]
allVertices (RefS _ v children) = v : concatMap allVertices children
allVertices (Application name v lst) = [to | (_, to, _) <- lst]  -- solo destinos

-- Cuenta la cantidad de vértices 
countVertices :: RefHFScheme t -> Int
countVertices refhfs = length . nub $ allVertices refhfs

-- Busca el nodo asociado a un vértice
getVertexById :: RefHFScheme t -> Vertex -> RefHFScheme t
getVertexById node@(RefU (_, v)) target
  | v == target = node
  | otherwise   = error $ "Vértice no encontrado: " ++ show target
getVertexById node@(Application name v lst) target
  | any (\(_, to, _) -> to == target) lst = node
  | otherwise = error $ "Vértice no encontrado: " ++ show target
getVertexById node@(RefS _ v children) target
  | v == target = node
  | otherwise   = searchChildren children
  where
    searchChildren [] = error $ "Vértice no encontrado: " ++ show target
    searchChildren (c:cs) =
      if target `elem` allVertices c then getVertexById c target else searchChildren cs

-- Obtiene la etiqueta de un vértice
getNodeLabelById :: RefHFScheme Label -> Vertex -> Label
getNodeLabelById node@(RefU (l, v)) target
  | v == target = l
  | otherwise   = error $ "Vértice no encontrado: " ++ show target
getNodeLabelById node@(RefS l v children) target
  | v == target = l
  | otherwise   = searchChildren children
  where
    searchChildren [] = error $ "Vértice no encontrado: " ++ show target
    searchChildren (c:cs) =
      if target `elem` allVertices c then getNodeLabelById c target else searchChildren cs

-- Construye la etiqueta de una arista
edgeLabel :: Name -> String -> Value -> String
edgeLabel name lbl v =
  name ++ "(" ++ lbl ++ ")" ++ membershipSymbol v

-- Símbolo según el valor lógico
membershipSymbol :: Value -> String
membershipSymbol V1 = "in"
membershipSymbol V0 = "not-in"
membershipSymbol V2 = "in!"

-- Devuelve las aristas salientes de un vértice
getChildren :: RefHFScheme Label -> Vertex -> [(Maybe Label, Vertex)]
getChildren (RefU (_, _)) _ = []
getChildren (Application name v lst) _ = [(Just (name ++ "(" ++ lbl ++ ")"), to) | (lbl, to, _) <- lst]
getChildren (RefS _ v children) target
  | v == target = nub $ concatMap childEdges children
  | otherwise   = concatMap (\c -> if isApplication c then [] else getChildren c target) children -- medio sin sentido el otherwwise
  where
    childEdges :: RefHFScheme Label -> [(Maybe Label, Vertex)]
    childEdges (RefU (_, v')) = [(Nothing, v')]
    childEdges (RefS _ v' _)    = [(Nothing, v')]
    childEdges (Application name v lst) =
                                [ (Just (edgeLabel name lbl v), to)
                                | (lbl, to, v) <- lst
                                ]

    isApplication Application {} = True
    isApplication _         = False

-- | Construye grafo con etiquetas en aristas
setToGraph :: RefHFScheme Label -> Graph Label
setToGraph refhfs =
  let n = countVertices refhfs
  in listArray (0, n - 1) [getChildren refhfs v | v <- [0..n-1]]

-- | Construye el grafo con etiquetas en nodos
setToLabGraph :: RefHFScheme Label -> LabGraph Label Label
setToLabGraph refhfs =
  let g = setToGraph refhfs
  in LabGraph g (\v -> getNodeLabelById refhfs v)
