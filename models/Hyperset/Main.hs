module Hyperset.Main where
import System.Directory (createDirectoryIfMissing)
import Data.Array
import Hyperset.Types
import Hyperset.Decorator
import Hyperset.Pretty
import Hyperset.DotExport
import Hyperset.Examples
import Hyperset.SetToGraph
import Hyperset.DenoteSystem 

-- === Ejemplo de sistema de ecuaciones ===
system :: System String
system =
  [ Equation "q"   (SetOf [Expr "p", Ref "A"])
  , Equation "A" (SetOf [Expr "s", Ref "B"])
  , Equation "B"  (SetOf [Expr "t", Expr "u"])
  ]

-- Labels para variables y constantes
labeling :: Labeling String
labeling 0 = S [U "q"]
labeling 1 = S [U "p"]
labeling 2 = S [U "s", U "t", U "u"]
labeling 3 = S [U "s"]
labeling 4 = S [U "t", U "u"]
labeling 5 = S [U "t"]
labeling 6 = S [U "u"]
labeling _ = S [U "???"]

-- === Ejecuta todo el pipeline ===
runPipeline :: String -> System String -> Labeling String -> IO ()
runPipeline name sys labeling = do
  let outDir = "output_hyperset"
  createDirectoryIfMissing True outDir

  -- Etapa 1: Pasar el sistema a RefHFS
  let refhfs = denoteSystem sys "q"
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