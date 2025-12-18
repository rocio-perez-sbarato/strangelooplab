{-|
Module      : Hyperset.Types
Description : Módulo que contiene operaciones básica de conjuntos.
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.Operations where
import Hyperset.Types 


{- | Unión de conjuntos
Caso U: ponemos los U x en una lista para meterlos en un conjunto
Caso S: concatenamos las listas de los conjuntos
-}
unionHFS :: Eq t => HFS t -> HFS t -> HFS t
unionHFS (S xs) (S ys) = S (unionList xs ys)
unionHFS (S xs) y      = S (unionList xs [y]) 
unionHFS x      (S ys) = S (unionList [x] ys)
unionHFS x      y      = S (unionList [x] [y]) 

-- | Unión de listas sin repetir elementos 
unionList :: Eq t => [HFS t] -> [HFS t] -> [HFS t]
unionList xs ys = xs ++ [ y | y <- ys, y `notElem` xs ]

-- | Definición de conjunto vacío como S []
isEmpty :: HFS t -> Bool
isEmpty (S []) = True
isEmpty _      = False

-- | Pasa de RefHFS a HFS
justHereditary :: RefHFS a -> HFS a
justHereditary (RefU (x,z)) = U x
justHereditary (RefS x xs) = S newList 
    where 
        newList = map justHereditary xs 

