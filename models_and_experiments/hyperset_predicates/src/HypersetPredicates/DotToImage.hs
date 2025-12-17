module HypersetPredicates.DotToImage where 

import System.Process (callProcess)

-- | Convierte input.dot -> output.png usando neato
dotToPng :: FilePath -> FilePath -> IO ()
dotToPng dotFile outPng = do
  callProcess "neato" ["-Tpng", dotFile, "-o", outPng]