module HypersetGraph.Main where
import System.Directory (createDirectoryIfMissing)
import Data.Array
import HypersetGraph.Types
import HypersetGraph.Decorator
import HypersetGraph.Pretty
import HypersetGraph.DotExport
import HypersetGraph.Examples
import HypersetGraph.SetToGraph

-- === Ejecutor ===
runExample :: String -> LabGraph String -> IO ()
runExample name labgraph = do
  let outDir = "output_hyperset"
  createDirectoryIfMissing True outDir
  let decorations = computeDecorations labgraph
  writeFile (outDir ++ "/" ++ name ++ ".dot") (showGraphViz labgraph)
  putStrLn $ "\nArchivo " ++ name ++ ".dot generado en carpeta output/"
  putStrLn $ "Decoraciones para " ++ name ++ ":"
  mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decorations)

-- Ejemplo de HFS para probar
hfsExample :: RefHFS Int
hfsExample = RefS "root" 0 [RefU (1, "child1", 1), RefU (2, "child2", 2)]

hfsCiclico :: RefHFS Int
hfsCiclico = RefS "root" 0 [RefU (0, "child1", 0)]

-- === Main ===
main :: IO ()
main = do
  putStrLn "Ejemplos (decorado y .dot del grafo)"
  mapM_ (uncurry runExample) examples
  print (setToGraph hfsExample)
  print (setToGraph hfsCiclico)
  print (getLabels hfsExample)
  print (getLabels hfsCiclico)
  putStrLn (showGraphViz (setToLabGraph hfsExample))
  putStrLn (showGraphViz (setToLabGraph hfsCiclico))