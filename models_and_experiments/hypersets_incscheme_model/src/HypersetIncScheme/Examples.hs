module HypersetIncScheme.Examples where 
import HypersetIncScheme.Types
import HypersetIncScheme.Schemes

generalScheme :: Inclosure String 
generalScheme = Inclosure "Omega" "x" "d"

russellScheme :: Inclosure String 
russellScheme = Inclosure "V" "x" "p"