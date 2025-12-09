module Hyperset.SetToPicture where 

import Hyperset.Decorator 
import Hyperset.SetToGraph
import Hyperset.Types
import Data.Array

setToPicture :: RefHFS t -> Labeling Label -> LabGraph Label
setToPicture refhfs labeling =
    let labgraph     = setToLabGraph refhfs labeling
        decs         = computeDecorations labgraph
        newLabeling  = (decs !)
    in setToLabGraph refhfs newLabeling
