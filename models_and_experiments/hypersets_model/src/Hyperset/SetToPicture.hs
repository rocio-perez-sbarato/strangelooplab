{-|
Module      : Hyperset.SetToPicture
Description : Pasaje de conjuntos a grafo canónico, a grafo con decoraciones como labeling
Copyright   : (c) Rocío Perez Sbarato, 2026
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.SetToPicture where 

import Hyperset.Decorator 
import Hyperset.SetToGraph ( setToLabGraph )
import Hyperset.Types ( Labeling, RefHFS, LabGraph, Label )
import Data.Array ( (!) )

setToPicture :: RefHFS t -> Labeling Label -> LabGraph Label
setToPicture refhfs labeling =
    let labgraph     = setToLabGraph refhfs labeling
        decs         = computeDecorations labgraph
        newLabeling  = (decs !)
    in setToLabGraph refhfs newLabeling

--setToCanonical :: RefHFS t -> LabGraph t 
