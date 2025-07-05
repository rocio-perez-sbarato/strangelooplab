module Hyperset.Main where
import System.Directory (createDirectoryIfMissing)
import Data.Array
import Hyperset.Types
import Hyperset.Decorator
import Hyperset.Pretty
import Hyperset.DotExport
import Hyperset.SetToGraph
import Hyperset.DenoteSystem 

-- Ejemplitos 
system :: System String
system = 
  [ Equation "X" (SetOf [Ref "Y"])
  , Equation "Y" (SetOf [Ref "X"])
  ]

labeling :: Labeling String
labeling 0 = U "X"
labeling 1 = U "Y"
labeling _ = S [U "???"]

-- === Ejemplo de sistema de ecuaciones ===
system2 :: System String
system2 =
  [ Equation "X" (SetOf [Ref "X"])  ]

-- Labels para variables y constantes
labeling2 :: Labeling String
labeling2 0 = S [U "0"]                       
labeling2 _ = S [U "???"]

system3 :: System String
system3 =
  [ Equation "X" (SetOf [Ref "Y", Ref "Z"])
  , Equation "Y" (SetOf [Ref "X"])
  , Equation "Z" (SetOf [Expr "0"])
  ]

labeling3 :: Labeling String
labeling3 0 = S [U "X"]
labeling3 1 = S [U "Y"]
labeling3 2 = S [U "Z"]
labeling3 3 = S [U "0"]
labeling3 _ = S [U "???"]

-- === Ejecuta todo el pipeline ===
runPipeline :: String -> System String -> Labeling String -> IO ()
runPipeline name sys labeling = do
  let outDir = "output_hyperset"
  createDirectoryIfMissing True outDir

  -- Etapa 1: Pasar el sistema a RefHFS
  let refhfs = denoteSystem sys "X"
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