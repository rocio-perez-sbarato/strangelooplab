import System.Directory (createDirectoryIfMissing)
import Hyperset.Types 
import Hyperset.Decorator
import Hyperset.SetToGraph
import Hyperset.DenoteSystem
import Hyperset.Pretty
import Hyperset.DotExport
import Hyperset.Examples 
import Hyperset.DotToImage
import Data.Array
import System.IO (hFlush, stdout)         
import Text.Read (readMaybe)               
import Control.Monad (forM_) 

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

    let dotDir = "results/output_hypersets/dotFiles"
    let imgDir = "results/output_hypersets/images"
    createDirectoryIfMissing True dotDir
    createDirectoryIfMissing True imgDir

    ----------------------------------------------------------------------
    -- Grafo sin decorar (cada nodo con su etiqueta original)
    ----------------------------------------------------------------------
    let basicLabGraph = setToLabGraph set labeling
    let basicGraphViz = showLabGraphViz basicLabGraph

    let basicDotFile = dotDir ++ "/" ++ name ++ "_labels.dot"
    let basicImgFile = imgDir ++ "/" ++ name ++ "_labels.png"

    writeFile basicDotFile basicGraphViz
    dotToPng basicDotFile basicImgFile

    putStrLn "Visualización simple generada.\n"

    ----------------------------------------------------------------------
    -- Impresión de decoraciones
    ----------------------------------------------------------------------
    -- Cambiar por computeDecorationsShort si quiere mayor legilibilidad
    let decs = computeDecorations basicLabGraph

    putStrLn "Decoraciones del grafo:\n"
    mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decs)

    putStrLn ""


-- lista de ejemplos disponibles
examplesList :: [(String, System String, String, Labeling String)]
examplesList =
  [ ("omega"           , omega            , "X", omegaLabeling)
  , ("omega2"          , omega2           , "X", omegaLabeling2)
  , ("mutual_ref", mutualRef, "X", mutualRefLabeling)
  , ("well_founded"    , wellFoundedSystem, "A", wellFoundedLabeling)
  , ("well_founded2"    , wellFounded2System, "X", wellFounded2Labeling)
  , ("non_well_founded", nonWellFoundedSystem, "X", nonWellFoundedLabeling)
  , ("non_well_founded2", nonWellFounded2System, "X", nonWellFounded2Labeling)
  , ("non_well_founded3", nonWellFounded3System, "A", nonWellFounded3Labeling)
  , ("non_well_founded4", nonWellFounded4System, "Z", nonWellFounded4Labeling)
  , ("non_well_founded5", nonWellFounded5System, "X", nonWellFounded5Labeling)
  , ("pair_canonical", pairSystem, "q", pairLabeling)
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