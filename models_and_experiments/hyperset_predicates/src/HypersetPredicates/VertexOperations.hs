module HypersetPredicates.VertexOperations where 

import HypersetPredicates.Types 

-- =======================================
-- | Devuelve el int asociado a una variable
-- =======================================
lookupList :: String -> [(String, Vertex)] -> Vertex
lookupList key vmap =
    case lookup key vmap of
      Just v  -> v
      Nothing -> error $ "variable no encontrada -> " ++ key

-- =======================================
-- | Construye mapping de variables a vértices
-- =======================================
getSetExprElements :: System String -> [String]
getSetExprElements = uniq . concatMap extractFromEquation 
  where
    extractFromEquation (Equation _ expr) = extractFromSetExpr expr
    extractFromEquation _                 = []

    extractFromSetExpr (Expr t)      = [t]
    extractFromSetExpr (Ref _)       = []
    extractFromSetExpr (SetOf exprs) = concatMap extractFromSetExpr exprs
    extractFromSetExpr (Pred _)      = []

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs)
  | x `elem` xs = uniq xs
  | otherwise   = x : uniq xs

buildVertexMap :: System String -> [(String, Vertex)]
buildVertexMap system =
  let varNames     = [v | Equation v _ <- system]
      varMap       = zip varNames [0..]
      exprElements = getSetExprElements system
      elementNames = filter (`notElem` varNames) exprElements
      exprMap      = zip elementNames [length varMap ..]
  in varMap ++ exprMap