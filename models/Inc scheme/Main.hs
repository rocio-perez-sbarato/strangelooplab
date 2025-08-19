module Main where

import Data.Graph ()
import Data.Array (assocs)
import Viz ( showGraphViz ) 
import Example ( rusellScheme, genInclosure, buildIncGraph, buildIncLabeling)
import Elements ( LabGraph(LabGraph) )
import System.Directory (createDirectoryIfMissing)
import System.IO

saveGraphViz :: FilePath -> LabGraph String String -> IO ()
saveGraphViz path graph =
    withFile path WriteMode $ \h -> do
        hSetEncoding h utf8
        hPutStr h (showGraphViz graph)

-- Programa principal
main :: IO ()
main = do
  let outDir = "output_inclosure" 
  let gr = buildIncGraph rusellScheme
  let lab = buildIncLabeling rusellScheme
  createDirectoryIfMissing True outDir
  let labgraph = LabGraph gr lab 
  let dotFile = outDir ++ "/" ++ "example" ++ ".dot"
  saveGraphViz dotFile labgraph
  putStrLn "Archivo rusell.dot generado!"