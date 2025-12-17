import System.Directory (createDirectoryIfMissing)
import HypersetPredicates.Types
import HypersetPredicates.ToDot
import Hyperset.DotToImage
import HypersetPredicates.Examples
import HypersetPredicates.Schemes
import HypersetPredicates.DenoteSystem 
import HypersetPredicates.SetToLabGraph

schemes :: [(String, Inclosure String, Variable)]
schemes =
    [ ("russell", russellScheme, "V")
    , ("general", generalScheme, "Omega")
    ]

processInclosure :: (String, Inclosure String, Variable) -> IO ()
processInclosure (name, inclosure, root) = do
    putStrLn $ "Procesando " ++ name

    -- Caso normal
    pipelineSystemToGraph name (schemeToSystem inclosure) root

    -- Caso borde
    pipelineSystemToGraph (name ++ "-border")
                (schemeBorderCase inclosure) root

pipelineSystemToGraph :: String -> System String -> Variable -> IO ()
pipelineSystemToGraph name system root = do
    let set = denoteSystem system root
    let dot = showLabGraphViz (setToLabGraph set)

    let baseDir = "results/output_hypersetspredicates"
    let dotDir  = baseDir ++ "/dotFiles"
    let imgDir  = baseDir ++ "/images"
    let dotFile = dotDir ++ "/" ++ name ++ ".dot"
    let imgFile = imgDir ++ "/" ++ name ++ ".png"

    createDirectoryIfMissing True dotDir
    createDirectoryIfMissing True imgDir
    writeFile dotFile dot
    dotToPng dotFile imgFile

    putStrLn $ "Diagrama del Inclosure Scheme para el caso " ++ name ++ " generado.\n."

main :: IO ()
main = do
    putStrLn "== Scheme a Diagrama ==\n"
    mapM_ processInclosure schemes
    putStrLn "\nTodos los esquemas fueron generados."