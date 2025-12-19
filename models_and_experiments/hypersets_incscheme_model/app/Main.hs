import System.Directory (createDirectoryIfMissing)
import HypersetIncScheme.Types
import HypersetIncScheme.ToDot
import Hyperset.DotToImage
import HypersetIncScheme.Examples
import HypersetIncScheme.Schemes
import HypersetIncScheme.DenoteSystem 
import HypersetIncScheme.SetToGraph

schemes :: [(String, Inclosure String, Variable)]
schemes =
    [ ("russell", russellScheme, "V")
    , ("general", generalScheme, "Omega")
    ]

processInclosure :: (String, Inclosure String, Variable) -> IO ()
processInclosure (name, inclosure, root) = do

    -- Caso normal
    pipelineSystemToGraph name (schemeToSystem inclosure) root

    -- Caso borde
    pipelineSystemToGraph (name ++ "-border")
                (schemeBorderCase inclosure) root

pipelineSystemToGraph :: String -> System String -> Variable -> IO ()
pipelineSystemToGraph name system root = do
    let set = denoteSystem system root
    let dot = showLabGraphViz (setToLabGraph set)

    let baseDir = "results/output_hypersets_incscheme"
    let dotDir  = baseDir ++ "/dotFiles"
    let imgDir  = baseDir ++ "/images"
    let dotFile = dotDir ++ "/" ++ name ++ ".dot"
    let imgFile = imgDir ++ "/" ++ name ++ ".png"

    createDirectoryIfMissing True dotDir
    createDirectoryIfMissing True imgDir
    writeFile dotFile dot
    dotToPng dotFile imgFile

    putStrLn $ "Diagrama del Inclosure Scheme para el caso " ++ name ++ " generado.\n"

main :: IO ()
main = do
    putStrLn "== Scheme a Diagrama ==\n"
    mapM_ processInclosure schemes
    putStrLn "Todos los esquemas fueron generados."