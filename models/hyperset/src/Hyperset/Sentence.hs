{- |
Module      : Hyperset.Paradox
Description : Generalización sintáctica de sentencias referenciales mediante el tipo RefHFS
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.Sentence
    ( Sentence (..),
        sentenceToSystem,
        sentenceLabelingEq,
        sentenceLabelingExpr,
        liar1, liar2,
        labelingCombined, 
        labelingCombinedList,
        dualLiarSystem,
        dualLiarLabeling,
        sentenceName,
        sentenceYablo,
        yabloFamilyLabeling,
        refE, refEq0, refq0, refq, ref0,
        yabloToSystem,
        yabloFamilySystem,
        buildLabeling,
        buildRefsList
    )
where

import System.Directory (createDirectoryIfMissing)
import Data.Array ( assocs )
import Hyperset.Decorator ( computeDecorations )
import Hyperset.SetToGraph ( setToLabGraph )
import Hyperset.DenoteSystem ( denoteSystem )
import Hyperset.Pretty ( prettyHFS )
import Hyperset.DotExport ( showLabGraphViz, showGraphViz )

import Hyperset.Types
    ( HFS(..), Equation(..), SetExpr(Expr, Ref, SetOf), System, Labeling) 
import Hyperset.Operations (unionHFS)
import Data.Char (isDigit)

-- * Tipo de datos de paradoja autorreferencial
{- | Representación abstracta de una paradoja autorreferencial,
como un par ordenado del tipo <predicate, subject, applicability>
-}
data Sentence = Sentence
    {   name:: String,  -- s1
        subject :: String,  -- s2
        predicate :: String, -- E
        applicability :: String -- 0
    } deriving Show

-- * Generalización de paradojas sin autorreferencia

{- | Funciones auxiliares para generar nombres de variables
adecuados a la sentencia. Notar la herencia en los nombres. 
-}
refE, refEq0, refq0, refq, ref0 :: Sentence -> String
refE    (Sentence _ _ pred _)     = pred ++ "_"
refEq0  (Sentence _ sub pred app) = pred ++ sub ++ app ++ "_"
refq0   (Sentence _ sub _ app)    = sub ++ app ++ "_"
refq    (Sentence _ sub _ _)      = sub ++ "_"
ref0    (Sentence _ _ _ app)      = app ++ "_"

-- | Traduce una Sentence a un sistema de ecuaciones general
sentenceToSystem :: Sentence -> System String
sentenceToSystem s@(Sentence name sub pred app) =
    let eRef    = refE s
        eq0Ref  = refEq0 s
        q0Ref   = refq0 s
        qRef    = refq s
        zeroRef = ref0 s
    in
    [ Equation (name ++ "_")    (SetOf [Ref eRef, Ref eq0Ref])
    , Equation eRef    (SetOf [Expr pred])
    , Equation eq0Ref  (SetOf [Ref eRef, Ref q0Ref])
    , Equation q0Ref   (SetOf [Ref qRef, Ref zeroRef])
    , Equation qRef    (SetOf [Ref (sub ++ "_")])
    , Equation zeroRef (SetOf [Expr app])
    ]

{- | Genera el Labeling a partir de una Sentence, incluyendo
    tanto ecuaciones como expresiones.
    El labeling generado es la función identidad del sistema de ecuaciones
    asociado a Sentence.
-}
-- | Genera un labeling para las expresiones de una Sentence.
sentenceLabelingExpr :: Sentence -> Labeling String
sentenceLabelingExpr (Sentence name sub pred app) = \ix ->
    case ix of
        0 -> S [U pred]
        1 -> S [U app]
        _ -> S [U "???"]

-- | Genera un labeling para las ecuaciones de una Sentence.
sentenceLabelingEq :: Sentence -> Labeling String
sentenceLabelingEq (Sentence name sub pred app) = \ix ->
    case ix of
        0 -> S [U name]
        1 -> S [S [U pred]]
        2 -> unionHFS (S [S [U pred]]) (S [S [U sub, U app]])
        3 -> S [unionHFS (S [U sub]) (S [U app])]
        4 -> S [S [U sub]]
        5 -> S [S [U app]]
        _ -> S [U "???"]

sentenceLabeling :: Sentence -> Labeling String
sentenceLabeling s ix
    | ix < 6 = sentenceLabelingEq s ix
    | otherwise = sentenceLabelingExpr s (ix - 6) 

{- | Crea un etiquetado combinado de dos sentencias.
    Se asume que cada una tiene 6 ecuaciones y 2 expresiones.
--}
labelingCombined :: Sentence -> Sentence -> Labeling String
labelingCombined s1 s2 ix
    | ix < eqSize = sentenceLabelingEq s1 ix
    | ix < 2 * eqSize = sentenceLabelingEq s2 (ix - eqSize)
    | ix < 2 * eqSize + exprSize = sentenceLabelingExpr s1 (ix - 2 * eqSize)
    | ix < 2 * (eqSize + exprSize) = sentenceLabelingExpr s2 (ix - 2 * eqSize - exprSize)
    | otherwise = S [U "???"]
    where
        eqSize = 6     
        exprSize = 2  

-- | Permite construir el labeling de un sistema completo con múltiples sentencias.
labelingCombinedList :: [(Int, Labeling String)] -> Labeling String
labelingCombinedList sizLabList ix = go 0 sizLabList
    where
        go _ [] = S [U "???"]
        go acc ((size, labeling):rest)
            | ix < acc + size = labeling (ix - acc)
            | otherwise       = go (acc + size) rest

-- * Paradojas referenciales

-- ** Paradoja del mentiroso dual

{- | El mentiroso dual. 
    q = "p", Q = "Es verdadera", 0 = False
    p = "q", P = "Es verdadera", 1 = True
-}

liar1 :: Sentence
liar1 = Sentence "p" "q" "E1" "0"

liar2 :: Sentence
liar2 = Sentence "q" "p" "E2" "1"

-- | Labeling de dual Liar
dualLiarLabeling :: Labeling String 
dualLiarLabeling = labelingCombined liar1 liar2 

-- | Sistema de ecuaciones de una paradoja compuesta por dos sentencias
dualLiarSystem :: System String
dualLiarSystem = sentenceToSystem liar1 ++ sentenceToSystem liar2

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

-- | Extrae el número de una sentencia a partir de su nombre 
getNumber :: String -> String 
getNumber = drop 1 
        
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

{- | Ejecuta el pipeline completo para un sistema:
   * Denotación del sistema
   * Construcción del grafo
   * Escritura del archivo DOT
   * Impresión de decoraciones
-}
runPipeline :: String -> System String -> Labeling String -> IO ()
runPipeline name sys labeling = do
    let outDir = "output_hyperset"
    createDirectoryIfMissing True outDir

    let refhfs = denoteSystem sys "s1"
    putStrLn $ "\nSistema de ecuaciones denotado para '" ++ name ++ "':"
    print refhfs

    let labgraph = setToLabGraph refhfs labeling
    let decorations = computeDecorations labgraph
    let dotFile = outDir ++ "/" ++ name ++ ".dot"
    writeFile dotFile (showLabGraphViz labgraph)

    putStrLn $ "\nGrafo en el archivo: " ++ dotFile
    putStrLn $ "\nDecoraciones para el sistema '" ++ name ++ "':"
    mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
            (assocs decorations)

main :: IO ()
main = do
    putStrLn "=== Hyperset Graph Pipeline ===\n"

    let sentYablo = sentenceYablo 1 3
    print sentYablo
    let sysYablo = yabloFamilySystem 3
    print sysYablo
    let labelingYablo = yabloFamilyLabeling 3

    putStrLn "\nSistema Yablo:"
    print sysYablo
    runPipeline "sistema_Yablo" sysYablo labelingYablo