{- |
Module      : Hyperset.Paradox
Description : Generalización sintáctica de paradojas autorreferenciales mediante el tipo RefHFS
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module HypersetParadox.BuildSelfRefSentence where
import Hyperset.Types
        ( HFS(..), Equation(..), SetExpr(Expr, Ref, SetOf), System, Labeling)
import Hyperset.Operations ( unionHFS ) 

-- * Tipo de datos de paradoja autorreferencial

{- | Representación abstracta de una paradoja autorreferencial,
como un par ordenado del tipo <predicate, subject, applicability>
-}
data Paradox = Paradox
    {   subject :: String,
        predicate :: String,
        applicability :: String
    }

-- * Generalización de paradojas autorreferenciales

-- | Traduce una Paradox a un sistema de ecuaciones general
paradoxToSystem :: Paradox -> System String
paradoxToSystem p@(Paradox sub pred app) =
    let eRef    = refE p
        eqq0Ref = refEqq0 p
        qq0Ref  = refQq0 p
        q0Ref   = refq0 p
        qRef    = refq p
        zeroRef = ref0 p
    in
    [ Equation sub    (SetOf [Ref eRef, Ref eqq0Ref])
    , Equation eRef    (SetOf [Expr pred])
    , Equation eqq0Ref  (SetOf [Ref eRef, Ref qq0Ref])
    , Equation qq0Ref  (SetOf [Ref sub, Ref q0Ref])    
    , Equation q0Ref   (SetOf [Ref qRef, Ref zeroRef])
    , Equation qRef    (SetOf [Ref sub])
    , Equation zeroRef (SetOf [Expr app])
    ]

{- | Genera el Labeling a partir de una Paradox. 
    El labeling generado es la función identidad del sistema de ecuaciones
    asociado a Paradox
-}
paradoxLabeling :: Paradox -> Labeling String
paradoxLabeling p@(Paradox sub pred app) = \ix ->
    case ix of
        0 -> S [U sub]
        1 -> S [U (refE p)]
        2 -> S [U (refEqq0 p)]
        3 -> S [U (refQq0 p)]
        4 -> S [U (refq0 p)]
        5 -> S [U (refq p)]
        6 -> S [U (ref0 p)]
        7 -> S [U pred]
        8 -> S [U app]
        _ -> S [U "???"]

{- | Funciones auxiliares para generar nombres de variables
adecuados a la sentencia. Notar la herencia en los nombres. 
-}
refE, refEqq0, refQq0, refq0, refq, ref0 :: Paradox -> String
refE    (Paradox _ pred _)      = pred ++ "_"
refEqq0 (Paradox sub pred app)  = pred ++ sub ++ sub ++ app ++ "_"
refQq0  (Paradox sub _ app)     = sub ++ sub ++ app ++ "_"
refq0   (Paradox sub _ app)     = sub ++ app ++ "_"
refq    (Paradox sub _ _)       = sub ++ "_"
ref0    (Paradox _ _ app)       = app ++ "_"