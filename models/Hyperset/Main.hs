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
mutualRef :: System String
mutualRef = 
  [ Equation "X" (SetOf [Ref "Y"])
  , Equation "Y" (SetOf [Ref "X"])
  ]

mutualRefLabeling :: Labeling String
mutualRefLabeling 0 = U "X"
mutualRefLabeling 1 = U "Y"
mutualRefLabeling _ = S [U "???"]

-- === Ejemplo de sistema de ecuaciones ===
omega :: System String
omega =
  [ Equation "X" (SetOf [Ref "X"])  ]

-- Labels para variables y constantes
omegaLabeling :: Labeling String
omegaLabeling 0 = S [U "0"]                       
omegaLabeling _ = S [U "???"]

wellFounded :: System String
wellFounded =
  [ Equation "X" (SetOf [Ref "Y", Ref "Z"])
  , Equation "Y" (SetOf [Ref "X"])
  , Equation "Z" (SetOf [Expr "0"])
  ]

wellFoundedLabeling :: Labeling String
wellFoundedLabeling 0 = S [U "X"]
wellFoundedLabeling 1 = S [U "Y"]
wellFoundedLabeling 2 = S [U "Z"]
wellFoundedLabeling 3 = S [U "0"]
wellFoundedLabeling _ = S [U "???"]

liar :: System String
liar = 
    [ 
        Equation "q" (SetOf [Ref "F", Ref "C"])
      , Equation "F" (SetOf [Expr "E"])
      , Equation "C" (SetOf [Ref "F", Ref "D"])
      , Equation "D" (SetOf [Ref "G", Ref "A"])
      , Equation "G" (SetOf [Ref "q"])
      , Equation "A" (SetOf [Expr "0"])
    ] 

liarLabeling :: Labeling String
liarLabeling 0 = S [U "q"]
liarLabeling 1 = S [S [U "E"]]
liarLabeling 2 = S [U "C"]
liarLabeling 3 = S [U "D"]
liarLabeling 4 = S [U "G"]
liarLabeling 5 = S [S [U "0"]]
liarLabeling 6 = S [U "E"]
liarLabeling 7 = S [U "0"]
liarLabeling _ = S [U "???"]

dualLiar :: System String
dualLiar = 
    [ 
        Equation "q" (SetOf [Ref "F", Ref "C"])
      , Equation "F" (SetOf [Expr "E"])
      , Equation "C" (SetOf [Ref "F", Ref "D"])
      , Equation "D" (SetOf [Ref "G", Ref "A"])
      , Equation "G" (SetOf [Ref "p"])
      , Equation "A" (SetOf [Expr "0"])
      , Equation "p" (SetOf [Ref "R", Ref "S"])
      , Equation "R" (SetOf [Expr "T"])
      , Equation "S" (SetOf [Ref "R", Ref "U"])
      , Equation "U" (SetOf [Ref "W", Ref "X"])
      , Equation "W" (SetOf [Ref "q"])
      , Equation "X" (SetOf [Expr "1"])
    ] 

dualLiarLabeling :: Labeling String
dualLiarLabeling 0 = S [U "q"]
dualLiarLabeling 1 = S [S [U "E"]]
dualLiarLabeling 2 = S [U "C"]
dualLiarLabeling 3 = S [U "D"]
dualLiarLabeling 4 = S [U "G"]
dualLiarLabeling 5 = S [S [U "0"]]
dualLiarLabeling 6 = S [U "p"]        
dualLiarLabeling 7 = S [S[U "T"]]        
dualLiarLabeling 8 = S [U "S"]        
dualLiarLabeling 9 = S [U "U"]        
dualLiarLabeling 10 = S [U "W"]        
dualLiarLabeling 11 = S [S[U "1"]]    
dualLiarLabeling 12 = S [U "E"]     
dualLiarLabeling 13 = S [U "0"]
dualLiarLabeling 14 = S [U "T"]
dualLiarLabeling 15 = S [U "1"]
dualLiarLabeling _ = S [U "???"]

systemM3 :: System String
systemM3 = 
    [ 
        Equation "M3" (SetOf [Ref "F", Ref "C"]) 
      , Equation "F" (SetOf [Expr "h"]) 
      , Equation "C" (SetOf [Ref "F", Ref "D"])
      , Equation "D" (SetOf [Ref "G", Ref "A"])
      , Equation "G" (SetOf [Ref "M3"])
      , Equation "A" (SetOf [Expr "1"])
    ] 

labelingM3 :: Labeling String
labelingM3 0 = S [U "M3"]
labelingM3 1 = S [S [U "h"]]
labelingM3 2 = S [U "C"]
labelingM3 3 = S [U "D"]
labelingM3 4 = S [U "G"]
labelingM3 5 = S [S [U "1"]]
labelingM3 6 = S [U "h"]
labelingM3 7 = S [U "1"]
labelingM3 _ = S [U "???"]

-- === Ejecuta todo el pipeline ===
runPipeline :: String -> System String -> Labeling String -> IO ()
runPipeline name sys labeling = do
  let outDir = "output_hyperset"
  createDirectoryIfMissing True outDir

  -- Etapa 1: Pasar el sistema a RefHFS
  let refhfs = denoteSystem sys "M3"
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
  print systemM3
  runPipeline "sistema" systemM3 labelingM3