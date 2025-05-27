{-# LANGUAGE TupleSections #-}

import Data.Array
import Data.List (intercalate)
import Data.Typeable (cast, Typeable)
import System.Directory (createDirectoryIfMissing)

-- === Tipo HFS ===
data HFS t = S [HFS t] | U t deriving (Eq, Show)

-- === Tipos de grafos ===
type Vertex = Int
type Graph = Array Vertex [Vertex]
type Edge = (Vertex, Vertex)
type Bounds = (Vertex, Vertex)

type Labeling a = Vertex -> a
data LabGraph n = LabGraph Graph (Labeling n) 

-- === Construcción del grafo ===
buildG :: Bounds -> [Edge] -> Graph
buildG bounds edges = accumArray (flip (:)) [] bounds [(v, w) | (v, w) <- edges]

-- === Decoración ===
decorateAll :: LabGraph String -> Array Vertex (HFS String)
decorateAll (LabGraph gr label) = decs
  where
    decs = listArray (bounds gr) [decorate [] v | v <- indices gr]
    decorate visited v
      | v `elem` visited = U (label v ++ " = {" ++ label v ++ "}")
      | otherwise =
          let children = gr ! v
              childDecs = map (decorate (v : visited)) children
          in S (childDecs ++ [U (label v)])

-- Pretty print
prettyHFS :: (Show t, Typeable t) => HFS t -> String
prettyHFS (U x) = case cast x of
    Just s  -> s
    Nothing -> show x  
prettyHFS (S xs) = "{" ++ intercalate ", " (map prettyHFS xs) ++ "}"

-- DOT export (solo con label simple)
showGraphViz :: (Show a, Typeable a) => LabGraph a -> String
showGraphViz (LabGraph gr label) =
  "digraph G {\n" ++
  concatMap showEdges (assocs gr) ++
  "}\n"
  where
    showNode v = show v 
    showEdges (v, ws) =
      concat [ "  " ++ show v ++ " -> " ++ show w ++ ";\n" | w <- ws ]

-- === Ejemplos ===

-- Ejemplo 1
edges1 :: [Edge]
edges1 = [ (0, 1), (0, 2), (1, 2) ]
labels1 :: Array Vertex String
labels1 = listArray (0, 2) ["x", "", "x,y"]
labGraph1 :: LabGraph String
labGraph1 = LabGraph (buildG (bounds labels1) edges1) (labels1 !)

-- Ejemplo 2
edges2 :: [Edge]
edges2 = [ (0, 1), (0, 2), (1, 3), (1, 4), (1, 2), (2, 3), (2, 1) ]
labels2 :: Array Vertex String
labels2 = listArray (0, 4) ["c", "a", "b", "maxi", "claire"]
labGraph2 :: LabGraph String
labGraph2 = LabGraph (buildG (bounds labels2) edges2) (labels2 !)

-- Ejemplo 3 (omega)
edges3 :: [Edge]
edges3 = [ (0, 0) ]
labels3 :: Array Vertex String
labels3 = listArray (0, 0) ["X"]
labGraph3 :: LabGraph String
labGraph3 = LabGraph (buildG (bounds labels3) edges3) (labels3 !)

-- Lista de ejemplos
type NamedExample = (String, LabGraph String)
examples :: [NamedExample]
examples =
  [ ("example1", labGraph1)
  , ("example2", labGraph2)
  , ("example3_cycle", labGraph3)
  ]

-- === Ejecutor ===
runExample :: String -> LabGraph String -> IO ()
runExample name labgraph = do
  let outDir = "output"
  createDirectoryIfMissing True outDir
  let decorations = decorateAll labgraph
  writeFile (outDir ++ "/" ++ name ++ ".dot") (showGraphViz labgraph)
  putStrLn $ "\nArchivo " ++ name ++ ".dot generado en carpeta output/"
  putStrLn $ "Decoraciones para " ++ name ++ ":"
  mapM_ (\(v, d) -> putStrLn $ "  " ++ show v ++ ": " ++ prettyHFS d)
        (assocs decorations)

-- === Main ===
main :: IO ()
main = do
  putStrLn "Corriendo todos los ejemplos:"
  mapM_ (uncurry runExample) examples
