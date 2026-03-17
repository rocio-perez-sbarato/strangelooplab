{-|
Module      : Hyperset.Equation
Description : Funciones que operan sobre el tipo Equation
Copyright   : (c) Rocío Perez Sbarato, 2026
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.Equation where 

import Hyperset.Types
    ( System, SetExpr, Equation(Equation), Variable ) 

-- | Diccionario variable -> expresión
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]