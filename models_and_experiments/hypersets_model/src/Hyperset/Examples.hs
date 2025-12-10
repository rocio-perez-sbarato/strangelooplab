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