module Hyperset.DotToImage where 
    
import System.Process (callProcess)
import System.Exit (ExitCode)

-- | Convierte input.dot -> output.png usando dot
dotToPng :: FilePath -> FilePath -> IO ()
dotToPng dotFile outPng = do
  callProcess "dot" ["-Tpng", dotFile, "-o", outPng]