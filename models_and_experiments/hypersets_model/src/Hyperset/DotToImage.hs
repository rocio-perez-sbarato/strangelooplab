{-|
Module      : Hyperset.DotToImage
Description : Pasaje de dot a imagen
Copyright   : (c) Rocío Perez Sbarato, 2026
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.DotToImage where 
    
import System.Process (callProcess)
import System.Exit (ExitCode)

-- | Convierte input.dot -> output.png usando dot
dotToPng :: FilePath -> FilePath -> IO ()
dotToPng dotFile outPng = do
  callProcess "dot" ["-Tpng", dotFile, "-o", outPng]