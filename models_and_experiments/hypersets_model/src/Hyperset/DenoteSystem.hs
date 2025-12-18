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
import Hyperset.Operations
import Hyperset.Vertex
import Hyperset.Equation
import System.IO.Unsafe 

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
    in RefU (t, v)

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

