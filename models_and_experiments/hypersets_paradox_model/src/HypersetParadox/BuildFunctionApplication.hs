module HypersetParadox.BuildFunctionApplication where 

import Hyperset.Types 

-- *************************************
-- | Inclosure Scheme
data Inclosure t = Inclosure
    { omega :: String               -- conjunto Ω "total"
    , x :: String                   -- conjunto x
    , label :: String               -- Función δ 
    }
    deriving (Show)

-- * Generalización de paradojas autorreferenciales

-- | Traduce una Paradox a un sistema de ecuaciones general
closureToSystem :: Inclosure String -> System String
closureToSystem i@(Inclosure omega x delta) =
    let dRef    = refD i 
        dqe0Ref = refDqe0 i
        qe0Ref  = refQe0 i 
        qRef    = refQ i 
        e0Ref   = refE0 i
        eRef    = refE i
        zeroRef = ref0 i
    in
    [ Equation "inc"    (SetOf [Ref dRef, Ref dqe0Ref])
    , Equation dRef     (SetOf [Expr delta])
    , Equation dqe0Ref  (SetOf [Ref dRef, Ref qe0Ref])
    , Equation qe0Ref   (SetOf [Ref qRef, Ref e0Ref])
    , Equation qRef     (SetOf [Expr x])
    , Equation e0Ref    (SetOf [Ref eRef, Ref zeroRef])
    , Equation eRef     (SetOf [Expr ("in" ++ x)])
    , Equation zeroRef  (SetOf [Expr "0"])
    ]

{- | Genera el Labeling a partir de una Paradox. 
    El labeling generado es la función identidad del sistema de ecuaciones
    asociado a Paradox
-}
closureLabeling :: Inclosure String -> Labeling String
closureLabeling (Inclosure omega x delta) = \ix ->
    case ix of
        0 -> S [U "inc"]
        1 -> S [S [U delta]]
        2 -> S [S [U delta], S [U x, U ("in" ++ x), U "0"]]
        3 -> S [S [U x], S [U ("in" ++ x), U "0"]]
        4 -> S [S [U x]]
        5 -> S [U ("in" ++ x), U "0"]
        6 -> S [S [U ("in" ++ x)]]
        7 -> S [S [U "0"]]
        8 -> S [U delta]
        9 -> S [U x]
        10 -> S [U ("in" ++ x)]
        11 -> S [U "0"]
        _ -> S [U "???"]

{- | Funciones auxiliares para generar nombres de variables
adecuados a la sentencia. Notar la herencia en los nombres. 
-}
refD, refDqe0, refQe0, refQ, refE0, refE, ref0 :: Inclosure String -> String
refD    (Inclosure  omega x delta) = delta ++  "_" 
refDqe0 (Inclosure  omega x delta) = delta ++ x ++ "in" ++ x ++ "0" ++ "_"
refQe0  (Inclosure  omega x delta) = delta ++ x ++ "0" ++ "_"
refQ    (Inclosure  omega x delta) = x ++ "_"
refE0   (Inclosure  omega x delta) = "in" ++ x ++ "0" ++ "_"
refE    (Inclosure  omega x delta) = "in" ++ x ++ "_"
ref0    (Inclosure  omega x delta) = "0" ++ "_"

-- | <delta, x, in x, 0>
