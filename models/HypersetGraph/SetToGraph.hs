module HypersetGraph.SetToGraph where 

import HypersetGraph.Types 
import HypersetGraph.Operations

import Data.Array

setToGraph :: HFS t -> Graph 
setToGraph hfs = listArray (0, lengthHFS hfs - 1) 
                  (map (getNeighbours (enumerateHFS hfs)) [0..lengthHFS hfs - 1])

enumerateHFS :: HFS t -> NumberedHFS t
enumerateHFS hfs = snd (enumerateFrom 0 hfs)
  where
    enumerateFrom :: Vertex -> HFS t -> (Vertex, NumberedHFS t)
    enumerateFrom n (U x) = (n + 1, NumberedU (x, n))
    enumerateFrom n (S xs) =
      let (n', xs') = enumerateHFSList (n + 1) xs
      in (n', NumberedS n xs')

    enumerateHFSList :: Vertex -> [HFS t] -> (Vertex, [NumberedHFS t])
    enumerateHFSList n [] = (n, [])
    enumerateHFSList n (x:xs) =
      let (n1, x') = enumerateFrom n x
          (n2, xs') = enumerateHFSList n1 xs
      in (n2, x' : xs')

getNeighbours :: NumberedHFS t -> Vertex -> [Vertex]
getNeighbours (NumberedU (x, v)) target = []
getNeighbours (NumberedS v children) target
  | v == target = map getVertex children
  | otherwise   = concat (map (\child -> getNeighbours child target) children)
  where
    getVertex (NumberedU (x, v)) = v
    getVertex (NumberedS v children) = v