module Hyperset.Vertex where 

import Hyperset.Types
import qualified Data.Set as Set

-- | Elimina duplicados conservando orden
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs)
    | x `elem` xs = uniq xs
    | otherwise   = x : uniq xs

{- | Cuenta los vértices únicos en el RefHFS
      Contempla ciclos y referencias entre vértices
      Evita repeticiones usando un Set
-}
countVertices :: RefHFS t -> Int
countVertices refhfs = Set.size (collectVertices refhfs Set.empty)
  where
    collectVertices (RefU (_, v)) seen
      | v `Set.member` seen = seen
      | otherwise           = Set.insert v seen
    collectVertices (RefS v children) seen
      | v `Set.member` seen = seen
      | otherwise           = foldr collectVertices (Set.insert v seen) children

-- =======================================
-- | Construye mapping de variables a vértices
-- =======================================
    
-- | Asigna vértices a variables y expr
buildVertexMap :: System String -> [(String, Vertex)]
buildVertexMap system =
    let 
        varNames = map (\(Equation v _) -> v) system
        varMap = zip varNames [0..]
        exprElements = getSetExprElements system
        -- Filtro de duplicados
        elementNames = filter (`notElem` varNames) exprElements
        exprMap = zip elementNames [length varMap ..]
    in varMap ++ exprMap

-- | Extrae todos las expr del sistema
getSetExprElements :: System String -> [String]
getSetExprElements system = uniq . concatMap extractFromEquation $ system
    where
        extractFromEquation (Equation _ expr) = extractFromSetExpr expr

        extractFromSetExpr (Expr t)      = [t]
        extractFromSetExpr (Ref _)       = []
        extractFromSetExpr (SetOf exprs) = concatMap extractFromSetExpr exprs

-- | A partir de una variable te da el número asociado
lookupList :: (Eq a, Show a) => a -> [(a, b)] -> b
lookupList k [] = error $ "No se encontró la clave: " ++ show k
lookupList k ((k', v):xs)
    | k == k'   = v
    | otherwise = lookupList k xs
