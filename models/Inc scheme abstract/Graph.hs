module Graph where

import Elements ( Graph, Inclosure(Inclosure), Labeling) 
import Data.Array ( array )

-- Grafo con aristas etiquetadas
buildIncGraph :: Inclosure -> Graph String
buildIncGraph (Inclosure n0 n1 epsi phi delta) =
  array (0,1)
    [ (0, [(phi, 0), (delta, 0)])
    , (1, [(epsi, 1), (delta, 1), ("⊆", 0)])
    ]

-- Etiquetas de nodos
buildIncLabeling :: Inclosure -> Labeling String
buildIncLabeling (Inclosure n0 n1 _ _ _) v =
  case v of
    0 -> n0
    1 -> n1
