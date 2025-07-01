module Hyperset.Pretty where

import Data.Typeable (cast, Typeable)
import Data.List (intercalate)
import Hyperset.Types

-- Pretty print
prettyHFS :: (Show t, Typeable t) => HFS t -> String
prettyHFS (U x) = case cast x of
    Just s  -> s -- No imprime los paréntesis de show
    Nothing -> show x  
prettyHFS (S []) = "{}"
prettyHFS (S xs) = "{" ++ intercalate ", " (map prettyHFS xs) ++ "}"