module HypersetGraph.Main where
import System.Directory (createDirectoryIfMissing)
import Data.Array
import HypersetGraph.Types
import HypersetGraph.Decorator
import HypersetGraph.Pretty
import HypersetGraph.DotExport
import HypersetGraph.Examples
import HypersetGraph.SetToGraph
import HypersetGraph.DenoteSystem 

-- === Ejemplo de sistema de ecuaciones ===
system :: System String
system =
  [ Equation "x" (SetOf [Ref "x"]) ] -- X = {X}

-- === Ejecuta todo el pipeline ===
runPipeline :: String -> System String -> IO ()
runPipeline name sys = do
  let outDir = "output_hyperset"
  createDirectoryIfMissing True outDir

  -- Etapa 1: Pasar el sistema a RefHFS
  let refhfs = denoteSystem sys "x"
  putStrLn $ "\nSistema de ecuaciones denotado para '" ++ name ++ "':"
  print refhfs

  -- Etapa 2: Conversión a grafo etiquetado
  let labgraph = setToLabGraph refhfs

  -- Etapa 3: Decoración
  let decorations = computeDecorations labgraph

  -- Etapa 4: Exportación a DOT
  let dotFile = outDir ++ "/" ++ name ++ ".dot"
  writeFile dotFile (showGraphViz labgraph)

  -- === Salida ===
  putStrLn $ "\nGrafo en el archivo: " ++ dotFile
  putStrLn $ "\nDecoraciones para el sistema '" ++ name ++ "':"
  mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decorations)

-- === Main ===
main :: IO ()
main = do
  putStrLn "=== Hyperset Graph Pipeline ===\n"
  putStrLn "Sistema de ejemplo:"
  print system
  runPipeline "sistema" system