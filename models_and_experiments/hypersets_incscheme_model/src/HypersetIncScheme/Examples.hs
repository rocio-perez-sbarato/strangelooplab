{- |
Module      : HypersetIncScheme.Examples
Copyright   : (c) Rocío Perez Sbarato, 2026
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module HypersetIncScheme.Examples where 
import HypersetIncScheme.Types ( Inclosure(Inclosure) )
import HypersetIncScheme.Schemes ()

generalScheme :: Inclosure String 
generalScheme = Inclosure "Omega" "x" "d"

russellScheme :: Inclosure String 
russellScheme = Inclosure "V" "x" "p"