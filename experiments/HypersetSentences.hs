import System.Directory (createDirectoryIfMissing)
import Data.Array
import Hyperset.Types
import Hyperset.Decorator
import Hyperset.SetToGraph
import Hyperset.DenoteSystem
import Examples 
import Hyperset.Pretty
import Hyperset.DotExport
import HypersetParadox.Sentence(sentenceYablo, yabloToSystem, yabloLabeling, dualLiarSystem, dualLiarLabeling)
import HypersetParadox.SelfRefParadox

-- | Ejecuta todo el pipeline
runPipeline :: String -> System String -> Labeling String -> IO ()
runPipeline name sys labeling = do
  let outDir = "output_hyperset"
  createDirectoryIfMissing True outDir

  let refhfs = denoteSystem sys "M3"
  putStrLn $ "\nSistema de ecuaciones denotado para '" ++ name ++ "':"
  print refhfs

  let labgraph = setToLabGraph refhfs labeling
  let decorations = computeDecorations labgraph
  let dotFile = outDir ++ "/" ++ name ++ ".dot"
  writeFile dotFile (showLabGraphViz labgraph)

  putStrLn $ "\nGrafo en el archivo: " ++ dotFile
  putStrLn $ "\nDecoraciones para el sistema '" ++ name ++ "':"
  mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decorations)

main :: IO ()
main = do
  putStrLn "=== Hyperset Graph Pipeline ===\n"

  -- Primer sistema: M3
  putStrLn "\nSistema M3:"
  print systemM3
  runPipeline "sistema_M3" systemM3 labelingM3

  -- Segundo sistema: Yablo 
  let sentYablo = sentenceYablo 1 2
  let sysYablo = yabloToSystem sentYablo
  let labelingYablo = yabloLabeling sentYablo

  putStrLn "\nSistema Yablo:"
  print sysYablo
  runPipeline "sistema_Yablo" sysYablo labelingYablo

  
  let sysDualLiar = dualLiarSystem 
  let labelingDualLiar = dualLiarLabeling

  putStrLn "\nSistema Dual Liar:"
  print sysDualLiar
  runPipeline "sistema_dual_liar" sysDualLiar labelingDualLiar