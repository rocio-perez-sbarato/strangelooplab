module IncSchemeFuncional.DenoteSystem where 
import IncSchemeFuncional.Types 
import IncSchemeFuncional.Schemes 

-- | Construye diccionario variable -> expresión
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]

-- | Construye diccionario de axiomas
buildAxiomDict :: System String -> [(Name, FunProp String)]
buildAxiomDict system = [(name, ax) | Axiom name ax <- system]

-- | Extrae todos los Expr de un sistema
getSetExprElements :: System String -> [String]
getSetExprElements system = uniq . concatMap extractFromEquation $ system
  where
    extractFromEquation (Equation _ expr) = extractFromSetExpr expr
    extractFromEquation _                 = []

    extractFromSetExpr (Expr t)      = [t]
    extractFromSetExpr (Ref _)       = []
    extractFromSetExpr (SetOf exprs) = concatMap extractFromSetExpr exprs
    extractFromSetExpr (Pred _)      = []

-- | Elimina duplicados conservando orden
uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs)
  | x `elem` xs = uniq xs
  | otherwise   = x : uniq xs

-- | Construye mapa variable -> vértice
buildVertexMap :: System String -> [(String, Vertex)]
buildVertexMap system =
  let varNames = [v | Equation v _ <- system]
      varMap   = zip varNames [0..]
      exprElements = getSetExprElements system
      elementNames = filter (`notElem` varNames) exprElements
      exprMap = zip elementNames [length varMap ..]
  in varMap ++ exprMap

-- | Busca clave en lista
lookupList :: (Eq a, Show a) => a -> [(a, b)] -> b
lookupList k [] = error $ "No se encontró la clave: " ++ show k
lookupList k ((k', v):xs)
  | k == k'   = v
  | otherwise = lookupList k xs

-- | Convierte un sistema a RefHFS procesando Pred con axiomas
denoteSystem :: System String -> String -> RefHFScheme String
denoteSystem system rootVar =
    let vmap   = buildVertexMap system
        dict   = buildDict system
        axdict = buildAxiomDict system
        v      = lookupList rootVar vmap
    in convertExpr vmap dict axdict [] [] 0 (rootVar, v) (Ref rootVar)

-- | Conversión recursiva de SetExpr -> RefHFS
convertExpr
  :: [(String, Vertex)]
  -> [(Variable, SetExpr String)]
  -> [(Name, FunProp String)]
  -> [Variable]
  -> [Variable]
  -> Int
  -> (Label, Vertex)
  -> SetExpr String
  -> RefHFScheme String

convertExpr vmap dict axdict visited expanded n (label, vertex) (SetOf exprs)
  | label `elem` expanded = RefU (label, vertex)
  | otherwise =
      let newExpanded = label : expanded
          convertedChildren = map (\e -> convertExpr vmap dict axdict visited newExpanded n (label, vertex) e) exprs
      in RefS vertex convertedChildren


-- === Expr ===
convertExpr vmap dict axdict visited expanded n (label, vertex) (Expr t) =
    let v = lookupList t vmap
    in RefU (t, v)

-- === Ref ===
convertExpr vmap dict axdict visited expanded n (label, vertex) (Ref var)
  | var `elem` visited =
      let v = lookupList var vmap
      in RefU (var, v)
  | otherwise =
      case lookup var dict of
        Just (SetOf es) ->
          let v = lookupList var vmap
              newVisited = var : visited
          in convertExpr vmap dict axdict newVisited expanded n (var, v) (SetOf es)
        Just expr ->
          let v = lookupList var vmap
          in convertExpr vmap dict axdict (var:visited) expanded n (var, v) expr
        Nothing ->
          let v = lookupList var vmap
          in RefU (var, v)


-- === Pred ===
convertExpr vmap dict axdict visited expanded n (label, vertex) (Pred name) =
    case lookup name axdict of
        Just axiom -> convertAxiomToApply vertex name axiom vmap
        Nothing    ->
            let idTo = lookupList name vmap
            in Apply [(name, idTo, V1)]  -- default: true


-- | Convierte FunProp del axioma en Apply
convertAxiomToApply
  :: Vertex -> Name -> FunProp String -> [(String, Vertex)] -> RefHFScheme String
convertAxiomToApply from label (And (Transcendence _ (Ref omega) (Expr l1))
                                    (Closure _ (Ref omega') (Expr l2)))
                                    vmap =
    let idOmega  = lookupList omega vmap
        idOmega' = lookupList omega' vmap
    in Apply 
         [ (l1, idOmega, V1)   -- Transcendence vale
         , (l2, idOmega', V1)   -- Closure vale
         ]

-- Si no matchea exactamente el patrón
convertAxiomToApply from label _ _ =
    Apply [(label, from, V2)]  -- contradicción o indefinido

-- example :: System String
-- example = schemeToSystem (Inclosure "omega" "x" "delta")

-- refExample :: RefHFScheme String
-- refExample = denoteSystem example "omega"

-- main :: IO ()
-- main = do
--     putStrLn "Sistema:"
--     print example
--     putStrLn "\nRefHFS resultante:"
--     print refExample
