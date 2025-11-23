module IncSchemeFuncional.DenoteSystem where
import IncSchemeFuncional.Types

-- =======================================
-- | Devuelve el int asociado a una variable
-- =======================================
lookupList :: String -> [(String, Vertex)] -> Vertex
lookupList key vmap =
    case lookup key vmap of
      Just v  -> v
      Nothing -> error $ "variable no encontrada -> " ++ key

-- =======================================
-- | Construcción de diccionarios
-- =======================================
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]

buildAxiomDict :: System String -> [(Name, FunProp String)]
buildAxiomDict system = [(name, ax) | Axiom name ax <- system]

-- =======================================
-- | Detecta contradicción p y not p
-- =======================================
isContradiction :: FunProp String -> FunProp String -> Bool
isContradiction p (Not q) = p == q
isContradiction _ _       = False

-- =======================================
-- | Extrae la variable destino de Elem o Not Elem
-- =======================================
getElemVar :: FunProp String -> String
getElemVar (Elem (Ref _) (Ref to))       = to
getElemVar (Not (Elem (Ref _) (Ref to))) = to
getElemVar _ = error "No es un Elem válido"

-- =======================================
-- | Evalúa FunProp simple -- lo llamaría más bien etiquetado
-- =======================================
evalProp :: FunProp String -> Value
evalProp (Elem _ _)        = V1 -- más general
evalProp (Not (Elem _ _))  = V0
evalProp _                 = V2

-- =======================================
-- | Convierte una sola propiedad de SetProp a Apply
-- =======================================
convertSingleProp :: [(String, Vertex)] -> Int -> FunProp String -> (String, Vertex, Value)
convertSingleProp vmap i prop =
    let toVar = getElemVar prop
        toVertex = lookupList toVar vmap
    in (show i ++ "_" ++ toVar, toVertex, evalProp prop) -- no me gusta el nombre

-- =======================================
-- | Conversión general a RefHFScheme
-- =======================================
denoteSystem :: System String -> String -> RefHFScheme String
denoteSystem system rootVar =
    let vmap   = buildVertexMap system
        dict   = buildDict system
        axdict = buildAxiomDict system
        v      = lookupList rootVar vmap
    in convertExprGeneral vmap dict axdict [] [] 0 (rootVar, v) (Ref rootVar)

-- =======================================
-- | Construye mapping de variables a vértices
-- =======================================
getSetExprElements :: System String -> [String]
getSetExprElements system = uniq . concatMap extractFromEquation $ system
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

-- =======================================
-- Conversión general de SetExpr a RefHFScheme
-- =======================================
convertExprGeneral
  :: [(String, Vertex)]                 -- vmap
  -> [(Variable, SetExpr String)]       -- dict
  -> [(Name, FunProp String)]           -- axdict
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
      in RefS vertex convertedChildren

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
    Just fprop -> 
      case fprop of
        SetProp props ->
          let contradictions = [(p,q) | p <- props, q <- props, isContradiction p q]
              applies = if not (null contradictions)
                        then [(name, fromVertex, V2)]
                        else [convertSingleProp vmap i p | (i,p) <- zip [1..] props]
              in Apply applies
    Nothing -> Apply [(name, fromVertex, V2)] -- podría haber otra label
