module HypersetGraph.Operations where

import HypersetGraph.Types

-- Al fin y al cabo, los conjuntos HFS son listas
-- Caso U: ponemos los U x en una lista para meterlos en un conjunto
-- Caso S: concatenamos las listas de los conjuntos
unionHFS :: Eq t => HFS t -> HFS t -> HFS t
unionHFS (S xs) (S ys) = S (unionList xs ys)
unionHFS (S xs) y      = S (unionList xs [y]) 
unionHFS x      (S ys) = S (unionList [x] ys)
unionHFS x      y      = S (unionList [x] [y]) 

-- Unión de listas sin repetir elementos 
unionList :: Eq t => [HFS t] -> [HFS t] -> [HFS t]
unionList xs ys = xs ++ [ y | y <- ys, y `notElem` xs ]

-- Definición de conjunto vacío como S []
isEmpty :: HFS t -> Bool
isEmpty (S []) = True
isEmpty _      = False

-- Cuenta todos los nodos
lengthHFS :: HFS t -> Int
lengthHFS (U _) = 1
lengthHFS (S xs) = 1 + sum (map lengthHFS xs)