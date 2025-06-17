module HypersetGraph.DenoteSystem where 

import HypersetGraph.Types 

-- Elimina duplicados conservando orden
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs)
  | x `elem` xs = uniq xs
  | otherwise   = x : uniq xs

-- Extrae todos los átomos del sistema
getAtoms :: System String -> [String]
getAtoms = uniq . concatMap (\(Equation _ e) -> go e)
  where
    go (Expr t)      = [t]
    go (Ref _)       = []
    go (SetOf exprs) = concatMap go exprs

-- Asigna vértices a variables y átomos
buildVertexMap :: System String -> [(String, Vertex)]
buildVertexMap system =
  let varNames = map (\(Equation v _) -> v) system
      varMap = zip varNames [0..]
      atomNames = filter (\a -> not (a `elem` varNames)) (getAtoms system)
      atomMap = zip atomNames [length varMap ..]
  in varMap ++ atomMap

-- Diccionario variable → expresión
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]

-- Lookup en una lista de pares clave-valor
lookupList :: Eq a => a -> [(a, b)] -> Maybe b
lookupList _ [] = Nothing
lookupList k ((k', v):xs)
  | k == k'   = Just v
  | otherwise = lookupList k xs

-- Conversión principal
convertExpr
  :: [(String, Vertex)]            -- vertexMap
  -> [(Variable, SetExpr String)] -- dict
  -> [Variable]                   -- visited
  -> [Variable]                   -- expanded
  -> Maybe (Label, Vertex)        -- contexto actual
  -> SetExpr String
  -> RefHFS String

convertExpr vmap dict visited expanded (Just (label, vertex)) (SetOf exprs)
  | label `elem` expanded = RefS label vertex []
  | otherwise = RefS label vertex (map (convertExpr vmap dict visited (label : expanded) Nothing) exprs)

convertExpr _ _ _ _ Nothing (SetOf _) =
  error "SetOf sin contexto de label/vertex"

convertExpr vmap _ _ _ _ (Expr t) =
  let Just v = lookupList t vmap
  in RefU (t, t, v)

convertExpr vmap dict visited expanded _ (Ref var)
  | var `elem` visited =
      let Just v = lookupList var vmap
      in RefU (var, var, v)
  | otherwise =
      case lookupList var dict of
        Just (SetOf es) ->
          let Just v = lookupList var vmap
          in convertExpr vmap dict (var : visited) expanded (Just (var, v)) (SetOf es)
        _ -> let Just v = lookupList var vmap in RefU (var, var, v)

denoteSystem :: System String -> String -> RefHFS String
denoteSystem system rootVar =
  let vmap = buildVertexMap system
      dict = buildDict system
  in case lookupList rootVar dict of
       Just expr ->
         let Just v = lookupList rootVar vmap
         in convertExpr vmap dict [] [] (Just (rootVar, v)) expr
       Nothing -> error $ "Variable " ++ rootVar ++ " no definida"