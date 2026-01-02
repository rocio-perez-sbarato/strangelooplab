{- |
Module      : Hyperset.Paradox
Description : Generalización sintáctica de sentencias referenciales mediante el tipo RefHFS
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module HypersetParadox.BuildSentence where
import Hyperset.Types
        ( HFS(..), Equation(..), SetExpr(Expr, Ref, SetOf), System, Labeling) 
import Hyperset.Operations (unionHFS)

-- * Tipo de datos de paradoja autorreferencial
{- | Representación abstracta de una paradoja autorreferencial,
como un par ordenado del tipo <predicate, subject, applicability>
-}
data Sentence = Sentence
    {   name:: String,  -- s1
        subject :: String,  -- s2
        predicate :: String, -- E
        applicability :: String -- 0
    } 

-- * Generalización de paradojas sin autorreferencia

{- | Funciones auxiliares para generar nombres de variables
adecuados a la sentencia. Notar la herencia en los nombres. 
-}
refE, refEqq0, refQq0, refq0, refq, ref0 :: Sentence -> String
refE    (Sentence _ _ pred _)      = pred ++ "_"
refEqq0 (Sentence _ sub pred app)  = pred ++ sub ++ sub ++ app ++ "_"
refQq0  (Sentence _ sub _ app)     = sub ++ sub ++ app ++ "_"
refq0   (Sentence _ sub _ app)     = sub ++ app ++ "_"
refq    (Sentence _ sub _ _)       = sub ++ "_"
ref0    (Sentence _ _ _ app)       = app ++ "_"

-- | Traduce una Sentence a un sistema de ecuaciones general
sentenceToSystem :: Sentence -> System String
sentenceToSystem s@(Sentence name sub pred app) =
    let eRef    = refE s
        eqq0Ref = refEqq0 s
        qq0Ref  = refQq0 s
        q0Ref   = refq0 s
        qRef    = refq s
        zeroRef = ref0 s
    in
    [ Equation name     (SetOf [Ref eRef, Ref eqq0Ref])
    , Equation eRef     (SetOf [Expr pred])
    , Equation eqq0Ref  (SetOf [Ref eRef, Ref qq0Ref])
    , Equation qq0Ref   (SetOf [Ref qRef, Ref q0Ref])    
    , Equation q0Ref    (SetOf [Ref qRef, Ref zeroRef])
    , Equation qRef     (SetOf [Ref sub])
    , Equation zeroRef  (SetOf [Expr app])
    ]

-- | Genera un labeling para las expresiones de una Sentence.
sentenceLabelingExpr :: Sentence -> Labeling String
sentenceLabelingExpr (Sentence name sub pred app) = \ix ->
    case ix of
        0 -> S [U pred]
        1 -> S [U app]
        _ -> S [U "???"]

-- | Genera un labeling para las ecuaciones de una Sentence.
sentenceLabelingEq :: Sentence -> Labeling String
sentenceLabelingEq s@(Sentence name sub pred app) = \ix ->
    case ix of
        0 -> S [U name]
        1 -> S [U (refE s)]
        2 -> S [U (refEqq0 s)]
        3 -> S [U (refQq0 s)]
        4 -> S [U (refq0 s)]
        5 -> S [U (refq s)]
        6 -> S [U (ref0 s)]
        _ -> S [U "???"]

{- | Genera el Labeling a partir de una Sentence, incluyendo
    tanto ecuaciones como expresiones.
    El labeling generado es la función identidad del sistema de ecuaciones
    asociado a Sentence.
-}
sentenceLabeling :: Sentence -> Labeling String
sentenceLabeling s ix
    | ix < eqSize = sentenceLabelingEq s ix
    | ix < eqSize + exprSize = sentenceLabelingExpr s (ix - eqSize)
    | otherwise = S [U "???"]
    where
        eqSize   = 7
        exprSize = 2

{- | Crea un etiquetado combinado de dos sentencias.
    Se asume que cada una tiene 6 ecuaciones y 2 expresiones.
--}
labelingCombined :: Sentence -> Sentence -> Labeling String
labelingCombined s1 s2 ix
    | ix < eqSize =
        sentenceLabelingEq s1 ix
    | ix < 2 * eqSize =
        sentenceLabelingEq s2 (ix - eqSize)
    | ix < 2 * eqSize + exprSize =
        sentenceLabelingExpr s1 (ix - 2 * eqSize)
    | ix < 2 * (eqSize + exprSize) =
        sentenceLabelingExpr s2 (ix - 2 * eqSize - exprSize)
    | otherwise =
        S [U "???"]
    where
        eqSize   = 7
        exprSize = 2

-- | Permite construir el labeling de un sistema completo con múltiples sentencias.
labelingCombinedList :: [(Int, Labeling String)] -> Labeling String
labelingCombinedList sizLabList ix = go 0 sizLabList
    where
        go _ [] = S [U "???"]
        go acc ((size, labeling):rest)
            | ix < acc + size = labeling (ix - acc)
            | otherwise       = go (acc + size) rest

