module HypersetGraph.Main where

import System.Directory (createDirectoryIfMissing)
import Data.Array
import HypersetGraph.Types
import HypersetGraph.Decorator
import HypersetGraph.Pretty
import HypersetGraph.DotExport
import HypersetGraph.Examples

-- === Ejecutor ===
runExample :: String -> LabGraph String -> IO ()
runExample name labgraph = do
  let outDir = "output"
  createDirectoryIfMissing True outDir
  let decorations = decorateAll labgraph
  writeFile (outDir ++ "/" ++ name ++ ".dot") (showGraphViz labgraph)
  putStrLn $ "\nArchivo " ++ name ++ ".dot generado en carpeta output/"
  putStrLn $ "Decoraciones para " ++ name ++ ":"
  mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decorations)

-- === Main ===
main :: IO ()
main = do
  putStrLn "Ejemplos (decorado y .dot del grafo)"
  mapM_ (uncurry runExample) examples
