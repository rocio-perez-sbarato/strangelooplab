{-|
Module      : Hyperset.Examples
Description : Ejemplos de sistemas de ecuaciones y labelings que representan semánticamente una paradoja
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.Examples
  ( mutualRef, mutualRefLabeling
  , omega, omegaSet, omegaLabeling
  , omega2, omegaSet2, omegaLabeling2
  , wellFoundedSystem, wellFoundedSet, wellFoundedLabeling
  , nonWellFoundedSystem, nonWellFoundedSet, nonWellFoundedLabeling
  , liar, liarLabeling
  , dualLiar, dualLabeling
  , systemM3, labelingM3
  ) where
import Hyperset.Types
      ( Equation(Equation),
        HFS(U, S),
        Labeling,
        SetExpr(Expr, Ref, SetOf),
        System,
        RefHFS(RefS, RefU) ) 
import GHC.Generics (R)

-- === Ejemplo mutualRef ===
mutualRef :: System String
mutualRef =
  [ Equation "X" (SetOf [Ref "Y"])
  , Equation "Y" (SetOf [Ref "X"])
  ]

mutualRefLabeling :: Labeling String
mutualRefLabeling 0 = U "X"
mutualRefLabeling 1 = U "Y"
mutualRefLabeling _ = S [U "???"]

-- === omega ===
omega :: System String
omega = [ Equation "X" (SetOf [Ref "X"]) ]

omegaSet :: RefHFS String
omegaSet = RefS 0 [RefU ("X",0)]

omegaLabeling :: Labeling String
omegaLabeling 0 = S [U "X"]
omegaLabeling _ = S [U "???"]

-- === omega2 ===
omega2 :: System String
omega2 = [ Equation "X" (SetOf [Ref "Y"]),
          Equation "Y" (SetOf [Ref "Y"])]

omegaSet2 :: RefHFS String
omegaSet2 = RefS 0 [RefS 1 [RefU ("Y",1)]]

omegaLabeling2 :: Labeling String
omegaLabeling2 0 = S [U "X"]
omegaLabeling2 1 = S [U "Y"]
omegaLabeling2 _ = S [U "???"]

-- === wellFounded ===
wellFoundedSystem :: System String
wellFoundedSystem =
  [ Equation "A" (SetOf [Ref "X", Ref "C"])
  , Equation "C" (SetOf [Ref "X"])
  , Equation "X" (SetOf [Expr "B"])
  ]

wellFoundedSet :: RefHFS String 
wellFoundedSet = RefS 0 [RefS 2 [RefU ("B",3)],RefS 1 [RefS 2 [RefU ("B",3)]]]

wellFoundedLabeling :: Labeling String
wellFoundedLabeling 0 = S [U "X"]
wellFoundedLabeling 1 = S [ ]
wellFoundedLabeling 2 = S [U "X", U "Y"]
wellFoundedLabeling 3 = S [U "Y"]
wellFoundedLabeling _ = S [U "???"]

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
nonWellFoundedLabeling 0 = S [U "X"]
nonWellFoundedLabeling 1 = S [U "Y"]
nonWellFoundedLabeling 2 = S [U "Z"]
nonWellFoundedLabeling 3 = S [U "0"]
nonWellFoundedLabeling _ = S [U "???"]

-- === liar ===
liar :: System String
liar = 
  [ Equation "q" (SetOf [Ref "F", Ref "C"])
  , Equation "F" (SetOf [Expr "E"])
  , Equation "C" (SetOf [Ref "F", Ref "D"])
  , Equation "D" (SetOf [Ref "G", Ref "A"])
  , Equation "G" (SetOf [Ref "q"])
  , Equation "A" (SetOf [Expr "0"])
  ]

liarLabeling :: Labeling String
liarLabeling 0 = S [U "q"]
liarLabeling 1 = S [S [U "E"]]
liarLabeling 2 = S [U "C"]
liarLabeling 3 = S [U "D"]
liarLabeling 4 = S [U "G"]
liarLabeling 5 = S [S [U "0"]]
liarLabeling 6 = S [U "E"]
liarLabeling 7 = S [U "0"]
liarLabeling _ = S [U "???"]

-- === dualLiar ===
dualLiar :: System String
dualLiar = 
  [ Equation "q" (SetOf [Ref "F", Ref "C"])
  , Equation "F" (SetOf [Expr "E"])
  , Equation "C" (SetOf [Ref "F", Ref "D"])
  , Equation "D" (SetOf [Ref "G", Ref "A"])
  , Equation "G" (SetOf [Ref "p"])
  , Equation "A" (SetOf [Expr "0"])
  , Equation "p" (SetOf [Ref "R", Ref "S"])
  , Equation "R" (SetOf [Expr "T"])
  , Equation "S" (SetOf [Ref "R", Ref "U"])
  , Equation "U" (SetOf [Ref "W", Ref "X"])
  , Equation "W" (SetOf [Ref "q"])
  , Equation "X" (SetOf [Expr "1"])
  ]

dualLabeling :: Labeling String
dualLabeling 0 = S [U "q"]
dualLabeling 1 = S [S [U "E"]]
dualLabeling 2 = S [U "C"]
dualLabeling 3 = S [U "D"]
dualLabeling 4 = S [U "G"]
dualLabeling 5 = S [S [U "0"]]
dualLabeling 6 = S [U "p"]
dualLabeling 7 = S [S [U "T"]]
dualLabeling 8 = S [U "S"]
dualLabeling 9 = S [U "U"]
dualLabeling 10 = S [U "W"]
dualLabeling 11 = S [S [U "1"]]
dualLabeling 12 = S [U "E"]
dualLabeling 13 = S [U "0"]
dualLabeling 14 = S [U "T"]
dualLabeling 15 = S [U "1"]
dualLabeling _ = S [U "???"]

-- === M3 ===
systemM3 :: System String
systemM3 = 
  [ Equation "M3" (SetOf [Ref "F", Ref "C"]) 
  , Equation "F" (SetOf [Expr "h"]) 
  , Equation "C" (SetOf [Ref "F", Ref "D"])
  , Equation "D" (SetOf [Ref "G", Ref "A"])
  , Equation "G" (SetOf [Ref "M3"])
  , Equation "A" (SetOf [Expr "1"])
  ]

labelingM3 :: Labeling String
labelingM3 0 = S [U "M3"]
labelingM3 1 = S [S [U "h"]]
labelingM3 2 = S [U "C"]
labelingM3 3 = S [U "D"]
labelingM3 4 = S [U "G"]
labelingM3 5 = S [S [U "1"]]
labelingM3 6 = S [U "h"]
labelingM3 7 = S [U "1"]
labelingM3 _ = S [U "???"]