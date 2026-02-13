{-|
Module      : Hyperset.Examples
Description : Ejemplos de sistemas de ecuaciones y labelings que representan semánticamente una paradoja
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.Examples where
import Hyperset.Types
      ( Equation(Equation),
        HFS(U, S),
        Labeling,
        SetExpr(Expr, Ref, SetOf),
        System,
        RefHFS(RefS, RefU) ) 

-- === Ejemplo mutualRef ===
mutualRef :: System String
mutualRef =
  [ Equation "X" (SetOf [Ref "Y"])
  , Equation "Y" (SetOf [Ref "X"])
  ]

mutualRefLabeling :: Labeling String
mutualRefLabeling 0 = U "X"
mutualRefLabeling 1 = U "Y"
mutualRefLabeling _ = U "???"

-- === omega ===
omega :: System String
omega = [ Equation "X" (SetOf [Ref "X"]) ]

omegaSet :: RefHFS String
omegaSet = RefS 0 [RefU ("X",0)]

omegaLabeling :: Labeling String
omegaLabeling 0 = U "X"
omegaLabeling _ = U "???"

-- === omega2 ===
omega2 :: System String
omega2 = [ Equation "X" (SetOf [Ref "Y"]),
          Equation "Y" (SetOf [Ref "Y"])]

omegaSet2 :: RefHFS String
omegaSet2 = RefS 0 [RefS 1 [RefU ("Y",1)]]

omegaLabeling2 :: Labeling String
omegaLabeling2 0 = U "X"
omegaLabeling2 1 = U "Y"
omegaLabeling2 _ = U "???"

-- === wellFounded ===
wellFoundedSystem :: System String
wellFoundedSystem =
  [ Equation "A" (SetOf [Expr "C", Ref "B"])
  , Equation "B" (SetOf [Expr "C"])
  ]

wellFoundedSet :: RefHFS String 
wellFoundedSet = RefS 0 [RefU ("C",2),RefS 1 [RefU ("C",2)]]

wellFoundedLabeling :: Labeling String
wellFoundedLabeling 0 = S [U "X"] -- A
wellFoundedLabeling 1 = S [ ]  -- B
wellFoundedLabeling 2 = S [U "X", U "Y"] -- C 
wellFoundedLabeling _ = U "???" 

-- === wellFounded2 ===
wellFounded2System :: System String
wellFounded2System =
  [ Equation "X" (SetOf [Ref "Y", Expr "Hola"])
  , Equation "Y" (SetOf [Ref "Z", Expr "Chau"])  
  , Equation "Z" (SetOf [Expr "3", Expr "5"])
  ]

wellFounded2Set :: RefHFS String 
wellFounded2Set = RefS 0 [RefS 1 [RefS 2 [RefU ("3",5),RefU ("5",6)],RefU ("Chau",4)],RefU ("Hola",3)]

wellFounded2Labeling :: Labeling String
wellFounded2Labeling 0 = U "X"
wellFounded2Labeling 1 = U "Y"
wellFounded2Labeling 2 = U "Z"
wellFounded2Labeling 3 = U "Hola"
wellFounded2Labeling 4 = U "Chau"
wellFounded2Labeling 5 = U "3"
wellFounded2Labeling 6 = U "5"
wellFounded2Labeling _ = U "???"

canonicalWellFounded2Labeling :: Labeling String
canonicalWellFounded2Labeling 0 = S [S [S [U "3",U "5"],U "Chau"],U "Hola"]
canonicalWellFounded2Labeling 1 = S [S [U "3",U "5"], U "Chau"]
canonicalWellFounded2Labeling 2 = S [U "3",U "5"]
canonicalWellFounded2Labeling 3 = U "Hola"
canonicalWellFounded2Labeling 4 = U "Chau"
canonicalWellFounded2Labeling 5 = U "3"
canonicalWellFounded2Labeling 6 = U "5"
canonicalWellFounded2Labeling _ = U "???"

-- === nonWellFounded ===
nonWellFoundedSystem :: System String
nonWellFoundedSystem =
  [ Equation "X" (SetOf [Ref "Y", Ref "Z"])
  , Equation "Y" (SetOf [Expr "0"])
  , Equation "Z" (SetOf [Expr "0", Ref "X"])
  ]

nonWellFoundedSet :: RefHFS String 
nonWellFoundedSet = RefS 0 [RefS 1 [RefU ("0",3)],RefS 2 [RefU ("0",3),RefU ("X",0)]]

nonWellFoundedLabeling :: Labeling String
nonWellFoundedLabeling 0 = U "X"
nonWellFoundedLabeling 1 = U "Y"
nonWellFoundedLabeling 2 = U "Z"
nonWellFoundedLabeling 3 = U "0"
nonWellFoundedLabeling _ = U "???"

canonicalNonWellFoundedLabeling :: Labeling String
canonicalNonWellFoundedLabeling 0 = S [S [U "0"],S [U "0",U "X"]]
canonicalNonWellFoundedLabeling 1 = S [U "0"]
canonicalNonWellFoundedLabeling 2 = S [U "0",U "X"]
canonicalNonWellFoundedLabeling 3 = U "0"
canonicalNonWellFoundedLabeling _ = U "???"

-- === nonWellFounded2 ===
nonWellFounded2System :: System String
nonWellFounded2System =
  [ Equation "X" (SetOf [Ref "Z", Ref "Y"])
  , Equation "Y" (SetOf [Ref "V"])
  , Equation "V" (SetOf [Ref "U"])  
  , Equation "U" (SetOf [Ref "X"])
  , Equation "Z" (SetOf [Expr "W", Ref "Y"])
  ]

nonWellFounded2Set :: RefHFS String 
nonWellFounded2Set = RefS 0 [RefS 4 [RefU ("W",5),RefS 1 [RefS 2 [RefS 3 [RefU ("X",0)]]]],RefS 1 [RefS 2 [RefS 3 [RefU ("X",0)]]]]

nonWellFounded2Labeling :: Labeling String
nonWellFounded2Labeling 0 = U "X"
nonWellFounded2Labeling 1 = U "Y"
nonWellFounded2Labeling 2 = U "V"
nonWellFounded2Labeling 3 = U "U"
nonWellFounded2Labeling 4 = U "Z"
nonWellFounded2Labeling 5 = U "W"
nonWellFounded2Labeling _ = U "???"


-- === nonWellFounded3 ===
nonWellFounded3System :: System String
nonWellFounded3System =
  [ Equation "A" (SetOf [Ref "B", Ref "C"])
  , Equation "B" (SetOf [Expr "Hola", Expr "Che", Ref "C"])
  , Equation "C" (SetOf [Expr "Chau", Expr "Che", Ref "B"])  
  ]

nonWellFounded3Set :: RefHFS String 
nonWellFounded3Set = RefS 0 [RefS 1 [RefU ("Hola",3),RefU ("Che",5),RefS 2 [RefU ("Chau",4),RefU ("Che",5),RefU ("B",1)]],RefS 2 [RefU ("Chau",4),RefU ("Che",5),RefS 1 [RefU ("Hola",3),RefU ("Che",5),RefU ("C",2)]]]

nonWellFounded3Labeling :: Labeling String
nonWellFounded3Labeling 0 = U "A"
nonWellFounded3Labeling 1 = U "B"
nonWellFounded3Labeling 2 = U "C"
nonWellFounded3Labeling 3 = U "Hola"
nonWellFounded3Labeling 4 = U "Che"
nonWellFounded3Labeling 5 = U "Chau"
nonWellFounded3Labeling _ = U "???"

canonicalNonWellFounded3Labeling :: Labeling String
canonicalNonWellFounded3Labeling 0 = S [S [U "Hola",U "Che", S [U "Chau",U "Che",U "B"]],S [U "Chau",U "Che",S [U "Hola",U "Che",U "C"]]]
canonicalNonWellFounded3Labeling 1 = S [U "Hola",U "Che",S [U "Chau",U "Che",U "B"]]
canonicalNonWellFounded3Labeling 2 = S [U "Chau",U "Che",S [U "Hola",U "Che",U "C"]]
canonicalNonWellFounded3Labeling 3 = U "Hola"
canonicalNonWellFounded3Labeling 4 = U "Che"
canonicalNonWellFounded3Labeling 5 = U "Chau"
canonicalNonWellFounded3Labeling _ = U "???"

-- === nonWellFounded4 ===
nonWellFounded4System :: System String
nonWellFounded4System =
  [ Equation "Z" (SetOf [Ref "X", Ref "Y"])
  , Equation "X" (SetOf [Expr "Hola", Ref "Y"])
  , Equation "Y" (SetOf [Expr "Chau", Ref "Z"])  
  ]

nonWellFounded4Set :: RefHFS String 
nonWellFounded4Set = RefS 0 [RefS 1 [RefU ("Hola",3),RefS 2 [RefU ("Chau",4),RefU ("Z",0)]],RefS 2 [RefU ("Chau",4),RefU ("Z",0)]]

nonWellFounded4Labeling :: Labeling String
nonWellFounded4Labeling 0 = U "Z"
nonWellFounded4Labeling 1 = U "X"
nonWellFounded4Labeling 2 = U "Y"
nonWellFounded4Labeling 3 = U "Hola"
nonWellFounded4Labeling 4 = U "Chau"
nonWellFounded4Labeling _ = U "???"

canonicalNonWellFounded4Labeling :: Labeling String
canonicalNonWellFounded4Labeling 0 = S [S [U "Hola",S [U "Chau",U "Z"]],S [U "Chau",U "Z"]]
canonicalNonWellFounded4Labeling 1 = S [U "Hola",S [U "Chau",U "Z"]]
canonicalNonWellFounded4Labeling 2 = S [U "Chau",U "Z"]
canonicalNonWellFounded4Labeling 3 = U "Hola"
canonicalNonWellFounded4Labeling 4 = U "Chau"
canonicalNonWellFounded4Labeling _ = U "???"

-- === nonWellFounded5 ===
nonWellFounded5System :: System String
nonWellFounded5System =
  [ Equation "X" (SetOf [Ref "X", Ref "Y"])
  , Equation "Y" (SetOf [Expr "A", Expr "B", Ref "Y", Ref "Z"])
  , Equation "Z" (SetOf [Ref "Z", Ref "X", Ref "Y"])  
  ]

nonWellFounded5Set :: RefHFS String 
nonWellFounded5Set = RefS 0 [RefU ("X",0),RefS 1 [RefU ("A",3),RefU ("B",4),RefU ("Y",1),RefS 2 [RefU ("Z",2),RefU ("X",0),RefU ("Y",1)]]] 

nonWellFounded5Labeling :: Labeling String
nonWellFounded5Labeling 0 = U "X"
nonWellFounded5Labeling 1 = U "Y"
nonWellFounded5Labeling 2 = U "Z"
nonWellFounded5Labeling 3 = U "A"
nonWellFounded5Labeling 4 = U "B"
nonWellFounded5Labeling _ = U "???"


-- === pair ===
pairSystem :: System String
pairSystem =
    [ Equation "q"    (SetOf [Ref "X", Ref "Y"])
    , Equation "X"    (SetOf [Expr "E"])
    , Equation "Y"    (SetOf [Ref "X", Ref "Z"])
    , Equation "Z"    (SetOf [Ref "U", Ref "V"])    
    , Equation "V"    (SetOf [Ref "q"])
    , Equation "U"    (SetOf [Ref "V", Ref "W"])
    , Equation "W"    (SetOf [Expr "0"])

    ]

pairSet :: RefHFS String
pairSet = RefS 0 [RefS 1 [RefU ("E",7)],RefS 2 [RefS 1 [RefU ("E",7)],RefS 3 [RefS 5 [RefS 4 [RefU ("q",0)],RefS 6 [RefU ("0",8)]],RefS 4 [RefU ("q",0)]]]]

canonicalPairLabeling :: Labeling String
canonicalPairLabeling 0 = S [S  [U "E"],S [S [U "E"],S [S [S [U "q"],S [U "0"]],S [U "q"]]]]
canonicalPairLabeling 1 = S [U "E"]
canonicalPairLabeling 2 = S [S [U "E"],S [S [S [U "q"],S [U "0"]],S [U "q"]]]
canonicalPairLabeling 3 = S [S [S [U "q"],S [U "0"]],S [U "q"]]
canonicalPairLabeling 4 = S [U "q"]
canonicalPairLabeling 5 = S [S [U "q"],S [U "0"]]
canonicalPairLabeling 6 = S [U "0"]
canonicalPairLabeling 7 = U "E"
canonicalPairLabeling 8 = U "0"
canonicalPairLabeling _ = U "???"

pairLabeling :: Labeling String
pairLabeling 0 = U "Q"
pairLabeling 1 = U "X"
pairLabeling 2 = U "Y"
pairLabeling 3 = U "Z"
pairLabeling 4 = U "V"
pairLabeling 5 = U "U"
pairLabeling 6 = U "W"
pairLabeling 7 = U "E"
pairLabeling 8 = U "0"
pairLabeling _ = U "???"