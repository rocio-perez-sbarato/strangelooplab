module Hyperset.DotToImage where 
    
import System.Process (callProcess)
import System.Exit (ExitCode)

-- convierte input.dot -> output.png usando dot
dotToPng :: FilePath -> FilePath -> IO ()
dotToPng dotFile outPng = do
  -- -Tpng formato de salida, -o archivo de salida
  callProcess "dot" ["-Tpng", dotFile, "-o", outPng]