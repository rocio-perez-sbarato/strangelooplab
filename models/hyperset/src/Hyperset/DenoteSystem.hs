{-|
Module      : Hyperset.Types
Description : Pasaje del tipo de datos System al tipo de datos HFS con una variable raíz.
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.DenoteSystem where 

import Hyperset.Types
    ( Equation(Equation),
      HFS,
      Label,
      RefHFS(..),
      SetExpr(..),
      System,
      Variable,
      Vertex ) 
import Hyperset.Operations ( justHereditary )
import System.IO.Unsafe (unsafePerformIO)

systemToHFS :: System String -> String -> HFS String
systemToHFS system rootVar = justHereditary (denoteSystem system rootVar)

denoteSystem :: System String -> String -> RefHFS String
denoteSystem system rootVar =
  let vmap = buildVertexMap system
      dict = buildDict system
      id = lookupList rootVar dict
      v = lookupList rootVar vmap
  in convertExpr vmap dict [] [] 0 (rootVar, v) id

-- Nuevo convertExpr con contador de Exprs
convertExpr
  :: [(String, Vertex)]            -- Mapa de variables a vértices
  -> [(Variable, SetExpr String)] -- Diccionario variable -> expresión
  -> [Variable]                    -- Variables visitadas
  -> [Variable]                    -- Variables expandidas
  -> Int                           -- Contador de Expr
  -> (Label, Vertex)               -- Contexto
  -> SetExpr String                -- Expresión a convertir
  -> RefHFS String

-- === SetOf ===
convertExpr vmap dict visited expanded n (label, vertex) (SetOf exprs)
  | label `elem` expanded = RefU (label, vertex)
  | otherwise =
      let newExpanded = label : expanded
          convertedChildren = map (\e -> convertExpr vmap dict visited newExpanded n (label, vertex) e) exprs
      in RefS vertex convertedChildren

-- === Expr ===
convertExpr vmap _ _ _ _ _ (Expr t) =
    let v = lookupList t vmap
    in (RefU (t, v))

-- === Ref ===
convertExpr vmap dict visited expanded n _ (Ref var)
  | var `elem` visited =
      let v = lookupList var vmap
      in RefU (var, v)
  | otherwise =
      case lookupList var dict of
        SetOf es ->
          let v = lookupList var vmap
              newVisited = var : visited
          in convertExpr vmap dict newVisited expanded n (var, v) (SetOf es)

-- | Extrae todos las expr del sistema
getSetExprElements :: System String -> [String]
getSetExprElements system = uniq . concatMap extractFromEquation $ system
  where
    extractFromEquation (Equation _ expr) = extractFromSetExpr expr

    extractFromSetExpr (Expr t)      = [t]
    extractFromSetExpr (Ref _)       = []
    extractFromSetExpr (SetOf exprs) = concatMap extractFromSetExpr exprs

-- | Elimina duplicados conservando orden
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs)
  | x `elem` xs = uniq xs
  | otherwise   = x : uniq xs

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

-- | Diccionario variable -> expresión
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]

-- | A partir de una variable te da el número asociado
lookupList :: (Eq a, Show a) => a -> [(a, b)] -> b
lookupList k [] = error $ "No se encontró la clave: " ++ show k
lookupList k ((k', v):xs)
  | k == k'   = v
  | otherwise = lookupList k xs