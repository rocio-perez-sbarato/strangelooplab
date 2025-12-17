module HypersetPredicates.Examples where 
import HypersetPredicates.Types
import HypersetPredicates.Schemes

generalScheme :: Inclosure String 
generalScheme = Inclosure "Omega" "x" "d"

russellScheme :: Inclosure String 
russellScheme = Inclosure "V" "x" "p"