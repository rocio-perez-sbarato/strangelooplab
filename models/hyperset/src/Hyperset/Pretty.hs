{-|
Module      : Hyperset.Types
Description : Tipos principales para representar sistemas de ecuaciones en ZFA
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}


module Hyperset.Pretty where

import Data.Typeable (cast, Typeable)
import Data.List (intercalate)
import Hyperset.Types

-- | Pretty print
prettyHFS :: (Show t, Typeable t) => HFS t -> String
prettyHFS (U x) = case cast x of
    Just s  -> s -- No imprime las comillas de show
    Nothing -> show x  
prettyHFS (S []) = "{}"
prettyHFS (S xs) = "{" ++ intercalate ", " (map prettyHFS xs) ++ "}"