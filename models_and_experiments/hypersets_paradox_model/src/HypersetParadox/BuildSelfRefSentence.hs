{- |
Module      : HypersetParadox.BuildSelfRefSentence
Description : Generalización sintáctica de paradojas autorreferenciales mediante el tipo RefHFS
Copyright   : (c) Rocío Perez Sbarato, 2026
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
data SelfRefSentence = SelfRefSentence
    {   subject :: String,
        predicate :: String,
        applicability :: String
    }

-- * Generalización de paradojas autorreferenciales

-- | Traduce una SelfRefSentence a un sistema de ecuaciones general
selfRefSentenceToSystem :: SelfRefSentence -> System String
selfRefSentenceToSystem p@(SelfRefSentence sub pred app) =
    let eRef    = refE p
        eqq0Ref = refEqq0 p
        qq0Ref  = refQq0 p
        q0Ref   = refq0 p
        qRef    = refq p
        zeroRef = ref0 p
    in
    [ Equation sub    (SetOf [Ref eRef, Ref eqq0Ref])
    , Equation eRef    (SetOf [Expr pred])
    , Equation eqq0Ref  (SetOf [Expr pred, Ref qq0Ref])
    , Equation qq0Ref  (SetOf [Ref qRef, Ref q0Ref])    
    , Equation q0Ref   (SetOf [Ref sub, Ref zeroRef])
    , Equation qRef    (SetOf [Ref sub])
    , Equation zeroRef (SetOf [Expr app])
    ]

{- | Genera el Labeling a partir de una SelfRefSentence. 
    El labeling generado es la función identidad del sistema de ecuaciones
    asociado a SelfRefSentence
-}
selfRefSentenceLabeling :: SelfRefSentence -> Labeling String
selfRefSentenceLabeling p@(SelfRefSentence sub pred app) = \ix ->
    case ix of
        0 -> U sub
        1 -> U (refE p)
        2 -> U (refEqq0 p)
        3 -> U (refQq0 p)
        4 -> U (refq0 p)
        5 -> U (refq p)
        6 -> U (ref0 p)
        7 -> U pred
        8 -> U app
        _ -> U "???"

{- | Funciones auxiliares para generar nombres de variables
adecuados a la sentencia. Notar la herencia en los nombres. 
-}
refE, refEqq0, refQq0, refq0, refq, ref0 :: SelfRefSentence -> String
refE    (SelfRefSentence _ pred _)      = pred ++ "_"
refEqq0 (SelfRefSentence sub pred app)  = pred ++ sub ++ sub ++ app 
refQq0  (SelfRefSentence sub _ app)     = sub ++ sub ++ app 
refq0   (SelfRefSentence sub _ app)     = sub ++ app 
refq    (SelfRefSentence sub _ _)       = sub ++ "_"
ref0    (SelfRefSentence _ _ app)       = app ++ "_"