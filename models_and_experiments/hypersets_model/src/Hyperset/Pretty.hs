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

getName :: HFS String -> String
getName (U x)   = x
getName (S [])  = "{}"
getName (S [U x]) = x 
getName (S xs)  = "{" ++ intercalate ", " (map getName xs) ++ "}"