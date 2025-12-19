module HypersetIncScheme.Equation where 

import HypersetIncScheme.Types 

-- | Diccionario variable -> expresión
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]