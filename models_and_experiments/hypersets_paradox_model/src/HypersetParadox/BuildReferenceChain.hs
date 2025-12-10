module HypersetParadox.BuildReferenceChain where 

import HypersetParadox.BuildSentence 
import Hyperset.Types
import Hyperset.Operations
import Data.Char (isDigit) 

-- ** Paradoja de Yablo

-- | Construcción de la sentencia i de la paradoja de Yablo con profundidad n 
sentenceYablo :: Int -> Int -> Sentence
sentenceYablo i n = Sentence (sentenceName i) (sentenceNamesRange i n) ("E" ++ show i) ("0" ++ show i)

-- | Añade el índice de la sentencia de la paradoja de Yablo
sentenceName :: Int -> String
sentenceName i = "s" ++ show i

-- | Construye el rango de nombres de sentencias desde i hasta n, separados por coma.
sentenceNamesRange :: Int -> Int -> String
sentenceNamesRange i n = concatMap (\j -> sentenceName j ++ if j < n then ", " else "") [i..n]

-- | Representa en formato HFS la lista de nombres desde i hasta n.
sentenceNamesHFS :: Int -> Int -> HFS String
sentenceNamesHFS i n = S [ S [ U (sentenceName j) ] | j <- [i..n] ]

-- | Extrae el número de una sentencia a partir de su nombre 
getNumber :: String -> String 
getNumber = drop 1 

-- | Obtiene el último número presente en el string del sujeto.
getLastNumber :: String -> Int
getLastNumber sub =
    let nums = [read n :: Int | n <- words $ map (\c -> if isDigit c then c else ' ') sub, not (null n)]
    in if null nums then error "No number found in sub" else last nums

{- | Función auxiliar para construir la lista de referencas a las sentencias s_i+1 ... s_n 
La sentencia i de la paradoja de Yablo con profundidad n les hace referencia.
-}
buildRefsList :: String -> String -> [SetExpr String]
buildRefsList name sub =
    let i = read (drop 1 name)
        jmax = getLastNumber sub
    in if i == jmax - 1
        then [Expr (sentenceName jmax)] 
        else [Ref (sentenceName j) | j <- [(i+1)..jmax-1]] ++ [Expr (sentenceName jmax)]

{- | Función auxiliar para construir la lista de labels de las sentencias s_i+1 ... s_n. 
La sentencia i de la paradoja de Yablo con profundidad n les hace referencia.
-}
buildLabeling :: String -> String -> HFS String
buildLabeling name sub = 
    let i = read (drop 1 name)       
        jmax = getLastNumber sub    
    in S [ S [ U (sentenceName j) ] | j <- [(i+1)..jmax] ]

{- | Pasaje de paradoja de Yablo a un sistema de ecuaciones. 
Notar la herencia en los nombres de variables, referencias y expresiones. 
-}
yabloToSystem :: Sentence -> System String
yabloToSystem s@(Sentence name sub pred app) =
    let eRef    = refE s
        eq0Ref  = refEq0 s
        q0Ref   = refq0 s
        qRef    = refq s
        zeroRef = ref0 s
    in
    [ Equation name   (SetOf [Ref eRef, Ref eq0Ref])
    , Equation eRef    (SetOf [Expr pred])
    , Equation eq0Ref  (SetOf [Ref eRef, Ref q0Ref])
    , Equation q0Ref   (SetOf [Ref qRef, Ref zeroRef])
    , Equation qRef    (SetOf (buildRefsList name sub))
    , Equation zeroRef (SetOf [Expr app])
    ]

{- | Creación del labeling para las ecuaciones de la sentencia i de la paradoja de Yablo con profundidad n. 
Notar la herencia en las etiquetas. -}
yabloLabelingEq :: Int -> Sentence -> Labeling String
yabloLabelingEq i (Sentence name sub pred app) = \ix ->
    case ix of
        0 -> S [U name]
        1 -> S [S [U pred]]
        2 -> unionHFS (S [S [U pred], S [U app]]) (sentenceNamesHFS (read (getNumber name)) (getLastNumber sub))
        3 -> unionHFS (sentenceNamesHFS (read (getNumber name)) (getLastNumber sub)) (S [S [U app]])
        4 -> buildLabeling name sub
        5 -> S [S [U app]]
        _ -> S[ U "???"]

{- | Creación del labeling para las expresiones de la sentencia i de la paradoja de Yablo con profundidad n. -}
yabloLabelingExpr :: Sentence -> Labeling String
yabloLabelingExpr (Sentence name sub pred app) = \ix ->
    case ix of
        0 -> S [U pred]
        1 -> S [U ("s" ++ show (getLastNumber sub))]
        2 -> S [U app]
        _ -> S [U "???"]

-- | Etiquetado combinado para la sentencia i de la paradoja de Yablo con profundidad n.
yabloLabeling :: Sentence -> Labeling String
yabloLabeling s@(Sentence name sub pred app) ix
    | ix < 6 = yabloLabelingEq (read (drop 1 name)) s ix
    | otherwise = sentenceLabelingExpr s (ix - 6) 

{-| Construcción del sistema de ecuaciones correspondiente a la paradoja de 
Yablo con profundidad n. La profunidad n significa que se detiene en la última sentencia 
(la sentencia n no tiene subsiguientes)
-}
yabloFamilySystem :: Int -> System String
yabloFamilySystem n = concat [yabloToSystem (sentenceYablo i n) | i <- [1..n-1]]

{- Construcción del labeling correspondiente a la paradoja de 
Yablo con profundidad n.
-}
yabloFamilyLabeling :: Int -> Labeling String
yabloFamilyLabeling n =
    let sentences     = [sentenceYablo i n | i <- [1..n-1]]
        labelingsEq   = [yabloLabelingEq i s | (i, s) <- zip [1..n-1] sentences]
        labelingsExprInit = [sentenceLabelingExpr s | s <- take (n - 2) sentences]
        labelingExprLast  = yabloLabelingExpr (sentences !! (n - 2))
        labelingsExpr     = labelingsExprInit ++ [labelingExprLast]
        exprSizes         = replicate (n - 2) 2 ++ [3]
    in labelingCombinedList $
        zip (repeat 6) labelingsEq ++ zip exprSizes labelingsExpr