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
        dqqe0Ref = refDqqe0 i
        qqe0Ref = refQqe0 i
        qe0Ref  = refQe0 i 
        qRef    = refQ i 
        e0Ref   = refE0 i
        eRef    = refE i
        zeroRef = ref0 i
    in
    [ Equation "c"       (SetOf [Ref dRef, Ref dqqe0Ref]) -- Raíz c de clausura
    , Equation dRef      (SetOf [Expr delta])
    , Equation dqqe0Ref  (SetOf [Ref dRef, Ref qqe0Ref])
    , Equation qqe0Ref   (SetOf [Ref qRef, Ref qe0Ref])
    , Equation qe0Ref    (SetOf [Ref qRef, Ref e0Ref])
    , Equation qRef      (SetOf [Expr x])
    , Equation e0Ref     (SetOf [Ref eRef, Ref zeroRef])
    , Equation eRef      (SetOf [Expr ("in" ++ x)])
    , Equation zeroRef   (SetOf [Expr "0"])
    ]

{- | Genera el Labeling a partir de una Paradox. 
    El labeling generado es la función identidad del sistema de ecuaciones
    asociado a Paradox
-}
closureLabeling :: Inclosure String -> Labeling String
closureLabeling i@(Inclosure omega x delta) = \ix ->
    case ix of
        0 -> S [U "c"]
        1 -> S [U (refD i)]
        2 -> S [U (refDqqe0 i)]
        3 -> S [U (refQqe0 i)]
        4 -> S [U (refQe0 i)]
        5 -> S [U (refQ i)]
        6 -> S [U (refE0 i)]
        7 -> S [U (refE i)]
        8 -> S [U (ref0 i)]
        9 -> S [U delta]
        10 -> S [U x]
        11 -> S [U ("in" ++ x)]
        12 -> S [U "0"]
        _ -> S [U "???"]

{- | Funciones auxiliares para generar nombres de variables
adecuados a la sentencia. Notar la herencia en los nombres. 
-}
refD, refDqqe0, refQqe0, refQe0, refQ, refE0, refE, ref0 :: Inclosure String -> String
refD    (Inclosure  omega x delta)  = delta ++  "_" 
refDqqe0 (Inclosure  omega x delta) = delta ++ x ++ x ++ "in" ++ x ++ "0" ++ "_"
refQqe0 (Inclosure  omega x delta)  = x ++ x ++ "in" ++ x ++ "0" ++ "_"
refQe0  (Inclosure  omega x delta)  = x ++ "in" ++ x ++ "0" ++ "_"
refQ    (Inclosure  omega x delta)  = x ++ "_"
refE0   (Inclosure  omega x delta)  = "in" ++ x ++ "0" ++ "_"
refE    (Inclosure  omega x delta)  = "in" ++ x ++ "_"
ref0    (Inclosure  omega x delta)  = "0" ++ "_"