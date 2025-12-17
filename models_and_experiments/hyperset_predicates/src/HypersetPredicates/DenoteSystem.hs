module HypersetPredicates.DenoteSystem where
import HypersetPredicates.Predicates 
import HypersetPredicates.VertexOperations 
import HypersetPredicates.Types
import Data.List

-- =======================================
-- | Construcción de diccionarios
-- =======================================
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]

-- =======================================
-- | Conversión general a RefHFScheme
-- =======================================
denoteSystem :: System String -> String -> RefHFScheme String
denoteSystem system rootVar =
    let vmap   = buildVertexMap system
        dict   = buildDict system
        axdict = buildFunctionDict system
        v      = lookupList rootVar vmap
    in convertExprGeneral vmap dict axdict [] [] 0 (rootVar, v) (Ref rootVar)

-- =======================================
-- Conversión general de SetExpr a RefHFScheme
-- =======================================
convertExprGeneral
  :: [(String, Vertex)]                 -- vmap
  -> [(Variable, SetExpr String)]       -- dict
  -> [(Name, FunApp String)]            -- app dict
  -> [Variable]                         -- visited
  -> [Variable]                         -- expanded
  -> Int                                -- n
  -> (String, Vertex)                   -- (label, vertex)
  -> SetExpr String                     -- expresión a convertir
  -> RefHFScheme String

-- === SetOf ===
convertExprGeneral vmap dict axdict visited expanded n (label, vertex) (SetOf exprs)
  | label `elem` expanded = RefU (label, vertex)
  | otherwise =
      let newExpanded      = label : expanded
          convertedChildren = map (\e -> convertExprGeneral vmap dict axdict visited newExpanded n (label, vertex) e) exprs
      in RefS label vertex convertedChildren

-- === Expr ===
convertExprGeneral vmap dict axdict visited expanded n (label, vertex) (Expr t) =
    let v = lookupList t vmap
    in RefU (t, v)

-- === Ref ===
convertExprGeneral vmap dict axdict visited expanded n (label, vertex) (Ref var)
  | var `elem` visited =
      let v = lookupList var vmap
      in RefU (var, v)
  | otherwise =
      case lookup var dict of
        Just (SetOf es) ->
          let v = lookupList var vmap
              newVisited = var : visited
          in convertExprGeneral vmap dict axdict newVisited expanded n (var, v) (SetOf es)
        Just expr ->
          let v = lookupList var vmap
          in convertExprGeneral vmap dict axdict (var:visited) expanded n (var, v) expr
        Nothing ->
          let v = lookupList var vmap
          in RefU (var, v)

-- === Pred ===
convertExprGeneral vmap dict axdict visited expanded n (label, fromVertex) (Pred name) =
  case lookup name axdict of
    Just (SetApp props) -> expandSetApp vmap name props fromVertex
    _                           -> Application name fromVertex [(name, fromVertex, V2)]