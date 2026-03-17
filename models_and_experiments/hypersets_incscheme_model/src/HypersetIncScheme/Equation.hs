{- |
Module      : HypersetIncScheme.Equation
Copyright   : (c) Rocío Perez Sbarato, 2026
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module HypersetIncScheme.Equation where 

import HypersetIncScheme.Types
    ( System, SetExpr, Variable, Statement(Equation) ) 

-- | Diccionario variable -> expresión
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]