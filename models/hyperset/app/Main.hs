{-|
Module      : Hyperset.Types
Description : Tipos principales para representar sistemas de ecuaciones en ZFA
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Main where

import System.Directory (createDirectoryIfMissing)
import Data.Array
import Hyperset.Types
import Hyperset.Decorator
import Hyperset.SetToGraph
import Hyperset.DenoteSystem
import Hyperset.Examples 
import Hyperset.Pretty
import Hyperset.DotExport

-- | Ejecuta todo el pipeline
runPipeline :: String -> System String -> Labeling String -> IO ()
runPipeline name sys labeling = do
  let outDir = "output_hyperset"
  createDirectoryIfMissing True outDir

  let refhfs = denoteSystem sys "M3"
  putStrLn $ "\nSistema de ecuaciones denotado para '" ++ name ++ "':"
  print refhfs

  let labgraph = setToLabGraph refhfs labeling
  let decorations = computeDecorations labgraph
  let dotFile = outDir ++ "/" ++ name ++ ".dot"
  writeFile dotFile (showLabGraphViz labgraph)

  putStrLn $ "\nGrafo en el archivo: " ++ dotFile
  putStrLn $ "\nDecoraciones para el sistema '" ++ name ++ "':"
  mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decorations)

main :: IO ()
main = do
  putStrLn "=== Hyperset Graph Pipeline ===\n"
  putStrLn "Sistema de ejemplo:"
  print systemM3
  runPipeline "sistema" systemM3 labelingM3