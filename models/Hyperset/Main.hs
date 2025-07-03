module Hyperset.Main where
import System.Directory (createDirectoryIfMissing)
import Data.Array
import Hyperset.Types
import Hyperset.Decorator
import Hyperset.Pretty
import Hyperset.DotExport
import Hyperset.SetToGraph
import Hyperset.DenoteSystem 

-- === Ejemplo de sistema de ecuaciones ===
system :: System String
system =
  [ Equation "a"   (SetOf [Ref "b", Ref "c"])
  , Equation "b" (SetOf [Ref "c"])
  , Equation "c"  (SetOf [])
  ]

-- Labels para variables y constantes
labeling :: Labeling String
labeling 0 = S [U "x"]
labeling 1 = S []
labeling 2 = S [U "x", U "y"]
labeling _ = S [U "???"]

-- === Ejecuta todo el pipeline ===
runPipeline :: String -> System String -> Labeling String -> IO ()
runPipeline name sys labeling = do
  let outDir = "output_hyperset"
  createDirectoryIfMissing True outDir

  -- Etapa 1: Pasar el sistema a RefHFS
  let refhfs = denoteSystem sys "a"
  putStrLn $ "\nSistema de ecuaciones denotado para '" ++ name ++ "':"
  print refhfs

  -- Etapa 2: Conversión a grafo etiquetado (usando labeling a mano)
  let labgraph = setToLabGraph refhfs labeling

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
  runPipeline "sistema" system labeling