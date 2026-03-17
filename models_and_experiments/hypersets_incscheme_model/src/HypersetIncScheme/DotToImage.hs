{- |
Module      : HypersetIncScheme.DotToImage
Copyright   : (c) Rocío Perez Sbarato, 2026
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}


module HypersetIncScheme.DotToImage where 

import System.Process (callProcess)

-- | Convierte input.dot -> output.png usando neato
dotToPng :: FilePath -> FilePath -> IO ()
dotToPng dotFile outPng = do
  callProcess "neato" ["-Tpng", dotFile, "-o", outPng]