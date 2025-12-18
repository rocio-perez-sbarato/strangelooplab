module HypersetPredicate.Examples where 
import HypersetPredicate.Types
import HypersetPredicate.Schemes

generalScheme :: Inclosure String 
generalScheme = Inclosure "Omega" "x" "d"

russellScheme :: Inclosure String 
russellScheme = Inclosure "V" "x" "p"