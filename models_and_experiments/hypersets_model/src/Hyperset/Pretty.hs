{-|
Module      : Hyperset.Types
Description : Módulo que realiza la impresión por pantalla del tipo de datos HFS. 
                Se encarga de visualizar este tipo como conjuntos en ZFA.
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.Pretty where
import Data.Typeable (cast, Typeable)
import Data.List (intercalate)
import Hyperset.Types ( HFS(..) )

-- | Pretty print
prettyHFS :: (Show t, Typeable t) => HFS t -> String
prettyHFS (U x) = case cast x of
    Just s  -> s -- No imprime las comillas de show
    Nothing -> show x  
prettyHFS (S []) = "{}"
prettyHFS (S xs) = "{" ++ intercalate ", " (map prettyHFS xs) ++ "}"

prettyVertex :: HFS String -> String
prettyVertex (U x)   = x
prettyVertex (S [])  = "{}"
prettyVertex (S [U x]) = x 
prettyVertex (S xs)  = "{" ++ intercalate ", " (map prettyVertex xs) ++ "}"