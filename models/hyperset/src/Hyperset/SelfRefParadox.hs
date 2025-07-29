{- |
Module      : Hyperset.Paradox
Description : Generalización sintáctica de paradojas autorreferenciales mediante el tipo RefHFS
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.SelfRefParadox
    ( Paradox (..),
        paradoxToSystem,
        paradoxLabeling,
        liarParadox,
        russellParadox,
        barberParadox,
        knownParadoxes,
    )
where

import Hyperset.Types
    ( HFS(..), Equation(..), SetExpr(Expr, Ref, SetOf), System, Labeling) 

-- * Tipo de datos de paradoja autorreferencial
{- | Representación abstracta de una paradoja autorreferencial,
como un par ordenado del tipo <predicate, subject, applicability>
-}
data Paradox = Paradox
    {   subject :: String,
        predicate :: String,
        applicability :: String
    }

-- * Generalización de @Paradox@

-- | Traduce una Paradox a un sistema de ecuaciones general
paradoxToSystem :: Paradox -> System String
paradoxToSystem (Paradox sub pred app) =
    [ Equation sub (SetOf [Ref "F", Ref "C"])
        , Equation "F" (SetOf [Expr pred])
        , Equation "C" (SetOf [Ref "F", Ref "D"])
        , Equation "D" (SetOf [Ref "G", Ref "A"])
        , Equation "G" (SetOf [Ref sub])
        , Equation "A" (SetOf [Expr app])
    ]

{- | Genera el Labeling a partir de una Paradox. 
    El labeling generado es la función identidad del sistema de ecuaciones
    asociado a Paradox
-}
paradoxLabeling :: Paradox -> Labeling String
paradoxLabeling (Paradox sub pred app) = \ix ->
    case ix of
        0 -> S [U sub]
        1 -> S [S [U pred]]
        2 -> S [U "C"]
        3 -> S [U "D"]
        4 -> S [U "G"]
        5 -> S [S [U app]]
        6 -> S [U pred]
        7 -> S [U app]
        _ -> S [U "???"]

-- * Paradojas

{- | El mentiroso. 
    q = "Esta oración", E = "Es verdadera", 0 = False
-}
liarParadox :: Paradox
liarParadox = Paradox "q" "E" "0"

{- | La paradoja de Russell
    R = "El conjunto de Russell", E = "Pertenece a sí mismo", 0 = False
-}
russellParadox :: Paradox
russellParadox = Paradox "R" "E" "0"

{- | Ejemplo: El barbero
    B = "El barbero", E = "Se afeita a sí mismo", 0 = False
-}
barberParadox :: Paradox
barberParadox = Paradox "B" "E" "0"

-- | Lista de paradojas disponibles
knownParadoxes :: [Paradox]
knownParadoxes =
    [ liarParadox,
        russellParadox,
        barberParadox
    ]
