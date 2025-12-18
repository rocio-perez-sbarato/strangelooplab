module HypersetPredicate.Equation where 

import HypersetPredicate.Types 

-- | Diccionario variable -> expresión
buildDict :: System t -> [(Variable, SetExpr t)]
buildDict system = [(v, e) | Equation v e <- system]