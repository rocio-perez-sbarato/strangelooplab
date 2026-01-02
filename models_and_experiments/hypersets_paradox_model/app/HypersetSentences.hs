import System.Directory (createDirectoryIfMissing)
import Hyperset.Types 
import Hyperset.Decorator
import Hyperset.SetToGraph
import Hyperset.SetToPicture
import Hyperset.DotToImage
import Hyperset.DenoteSystem
import Hyperset.Examples 
import Hyperset.Pretty
import Hyperset.DotExport
import HypersetParadox.BuildSentence
import HypersetParadox.BuildSelfRefSentence
import HypersetParadox.BuildReferenceChain
import HypersetParadox.BuildFunctionApplication
import HypersetParadox.Examples 
import System.IO (hFlush, stdout)         
import Text.Read (readMaybe)               
import Control.Monad (forM_) 
import Data.Array

pipelineSystemToSet :: String -> System String -> String -> Labeling String -> IO ()
pipelineSystemToSet name system rootVar labeling = do
    putStrLn "== Sistema --> Conjunto ==\n"

    print system 

    let set = denoteSystem system rootVar

    putStrLn "Conjunto generado\n"
    print set
    putStrLn "\n"

    pipelineSetToGraph name set labeling

pipelineSetToGraph :: String -> RefHFS String -> Labeling String -> IO ()
pipelineSetToGraph name set labeling = do

    putStrLn "== Conjunto --> Grafo ==\n"

    let dotDir = "results/output_hypersets_paradox/dotFiles"
    let imgDir = "results/output_hypersets_paradox/images"
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

    putStrLn "Grafo etiquetado simple generado.\n"

    ----------------------------------------------------------------------
    -- Picture de un conjunto 
    ----------------------------------------------------------------------
    let pictureLabGraph = setToPicture set labeling
    let pictureGraphViz = showLabGraphViz pictureLabGraph

    let pictureDotFile = dotDir ++ "/" ++ name ++ "_full_picture.dot"
    let pictureImgFile = imgDir ++ "/" ++ name ++ "_full_picture.png"

    writeFile pictureDotFile pictureGraphViz
    dotToPng pictureDotFile pictureImgFile

    let labeling = computeDecorationsShort basicLabGraph
    let shortPictureLabGraph = setToLabGraph set (labeling !)
    let shortPictureGraphViz = showLabGraphViz shortPictureLabGraph

    let shortPictureDotFile = dotDir ++ "/" ++ name ++ "_short_picture.dot"
    let shortPictureImgFile = imgDir ++ "/" ++ name ++ "_short_picture.png"

    writeFile shortPictureDotFile shortPictureGraphViz
    dotToPng shortPictureDotFile shortPictureImgFile

    putStrLn "Grafo visualización generado. Con ambos tipos de decorado, completo y abreviado.\n" 
    putStrLn $ "Revisar " ++ dotDir ++ " y " ++ imgDir ++ "\n"

    ----------------------------------------------------------------------
    -- Impresión de decoraciones
    ----------------------------------------------------------------------
    let decs = computeDecorations basicLabGraph

    putStrLn "Desarrollo de las decoraciones del grafo:\n"
    mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decs)

-- lista de ejemplos disponibles
examplesList :: [(String, System String, String, Labeling String)]
examplesList =
  [ ("liar"           , liarParadoxSystem , "q", liarParadoxLabeling)
  , ("dualLiar"       , dualLiarSystem , "p_", dualLiarLabeling)
  , ("yablo3"         , sysYablo3, "s1", labelingYablo3)
  , ("closureIncScheme" , closureIncSchemeSystem, "c", closureLabeling (Inclosure "omega" "x" "delta"))
  ]

-- menú interactivo
interactiveMenu :: IO ()
interactiveMenu = do
  putStrLn "=== Ejemplos del capítulo 4 ==="
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