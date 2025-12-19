module HypersetIncScheme.Predicate where
import HypersetIncScheme.Types
import HypersetIncScheme.Vertex
import Hyperset.Vertex
import Data.List

-- | Construye un diccionario de funciones a partir del sistema.
--   El resultado asocia cada nombre de función con su aplicación lógica.
buildFunctionDict :: System String -> [(Name, FunApp String)]
buildFunctionDict system = [(name, ax) | Function name ax <- system]

-- | Determina si dos aplicaciones son contradictorias.
isContradiction :: FunApp String -> FunApp String -> Bool
isContradiction p (Not q) = p == q
isContradiction (Not q) p = p == q
isContradiction _ _       = False

-- | Obtiene la información de las aplicaciones de predicados,
-- de dónde vienen, a dónde van y qué resultado tiene el predicado.
--   - Elem  (x ∈ y)  se interpreta como valor verdadero (V1)
--   - Not Elem (x !∈ y)   se interpreta como valor falso (V0)
getAppContainer :: FunApp String -> (String, String, Value)
getAppContainer (Elem (Ref from) (Ref to))       = (from, to, V1)
getAppContainer (Not (Elem (Ref from) (Ref to))) = (from, to, V0)
getAppContainer _ = error "No es una aplicación válida de una función"

-- | Convierte una aplicación lógica en un elemento de Application.
convertSingleApp :: [(String, Vertex)] -> FunApp String -> (String, Vertex, Value)
convertSingleApp vmap fun =       
    let (fromVar, toVar, value) = getAppContainer fun
        toVertex = lookupList toVar vmap
    in (fromVar, toVertex, value)

-- | Marca explícitamente el valor como inconsistente (V2),
--   indicando que la propiedad aparece afirmada y negada en el mismo conjunto.
simplifyContradiction :: [(String, Vertex)] -> FunApp String -> (String, Vertex, Value)
simplifyContradiction vmap fun =       
    let (fromVar, toVar, value) = getAppContainer fun
        toVertex = lookupList toVar vmap
    in (fromVar, toVertex, V2)

-- | Marca el conjunto de proposiciones sobre un conjunto 
-- como verdaderas, falsas o inconsistentes. Procesa las 
-- Sentences construidas con Function, pasandolas a Application
expandSetApp :: [(String, Vertex)]   -> Name -> [FunApp String] -> Vertex -> RefHFScheme String
expandSetApp vmap name props v =
  let contradictions =
        [(p,q) | p <- props, q <- props, isContradiction p q]

      contradictoryProps =
        nub [ x | (p,q) <- contradictions, x <- [p,q] ]

      consistent =
        [ p | p <- props, p `notElem` contradictoryProps ]

      applies =
          map (convertSingleApp vmap) consistent
        ++ map (simplifyContradiction vmap) contradictoryProps

  in Application name v applies