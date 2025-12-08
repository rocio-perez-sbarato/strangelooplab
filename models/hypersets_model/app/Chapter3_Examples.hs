import System.Directory (createDirectoryIfMissing)
import Hyperset.Types 
import Hyperset.Decorator
import Hyperset.SetToGraph
import Hyperset.DenoteSystem
import Hyperset.Pretty
import Hyperset.DotExport
import Hyperset.Examples 
import Data.Array
import System.IO (hFlush, stdout)         
import Text.Read (readMaybe)               
import Control.Monad (forM_) 

-- | Ejecuta todo el pipeline
pipelineSystemToSet :: String -> System String -> String -> Labeling String -> IO ()
pipelineSystemToSet name system rootVar labeling = do
    putStrLn "== Sistema --> Conjunto ==\n" 
    let set = denoteSystem system rootVar
    putStrLn "Conjunto generado\n"
    print set
    putStrLn "\n"
    pipelineSetToGraph name set labeling

pipelineSetToGraph :: String -> RefHFS String -> Labeling String -> IO ()
pipelineSetToGraph name set labeling = do

    putStrLn "== Conjunto --> Grafo ==\n" 
    
    let outDir = "experiments/output_hypersets"
    createDirectoryIfMissing True outDir

    let graph = setToGraph set 
    print graph
    putStrLn "\n"

    let labgraph = setToLabGraph set labeling
    let viz1 = showLabGraphViz labgraph
    let dotFile1 = outDir ++ "/" ++ name ++ "just_labels.dot"
    writeFile dotFile1 viz1


    let decorations = computeDecorations labgraph
    let apg = setToLabGraph set (decorations !)

    let viz = showLabGraphViz apg
    let dotFile = outDir ++ "/" ++ name ++ ".dot"
    writeFile dotFile viz

    putStrLn $ "Grafo en el archivo " ++ dotFile ++ "\n"
    putStrLn "Decoraciones para el grafo\n"
    mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decorations)
    putStrLn "\n"

-- lista de ejemplos disponibles
examplesList :: [(String, System String, String, Labeling String)]
examplesList =
  [ ("omega"           , omega            , "X", omegaLabeling)
  , ("omega2"          , omega2           , "X", omegaLabeling2)
  , ("well_founded"    , wellFoundedSystem, "A", wellFoundedLabeling)
  , ("non_well_founded", nonWellFoundedSystem, "X", nonWellFoundedLabeling)
  ]

-- menú interactivo
interactiveMenu :: IO ()
interactiveMenu = do
  putStrLn "=== Ejemplos del capítulo 3 ==="
  forM_ (zip [1..] examplesList) $ \(i,(name,_,_,_)) ->
    putStrLn $ show i ++ ") " ++ name
  putStrLn "a) run all"
  putStrLn "q) quit"
  putStr "Elección: "
  hFlush stdout
  sel <- getLine
  case sel of
    "q" -> putStrLn "Saliendo..."
    "a" -> mapM_ (\(n,s,r,l) -> pipelineSystemToSet n s r l) examplesList
    _   -> case readMaybe sel :: Maybe Int of
             Just k | k >= 1 && k <= length examplesList -> do
                       let (name,sys,root,label) = examplesList !! (k-1)
                       pipelineSystemToSet name sys root label
                       putStrLn "" >> interactiveMenu
             _ -> putStrLn "Opción inválida.\n" >> interactiveMenu

-- reemplazo main para usar el menú interactivo
main :: IO ()
main = interactiveMenu