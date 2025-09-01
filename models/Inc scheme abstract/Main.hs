module Main where

import Data.Graph ()
import Data.Array (assocs)
import Viz ( saveGraphViz ) 
import Graph ( buildIncGraph, buildIncLabeling ) 
import Paradox ( genInclosure )
import Elements ( LabGraph(LabGraph) )
import System.Directory (createDirectoryIfMissing)

main :: IO ()
main = do
  let outDir = "output_inclosure" 
  let gr = buildIncGraph genInclosure
  let lab = buildIncLabeling genInclosure
  createDirectoryIfMissing True outDir
  let labgraph = LabGraph gr lab 
  let dotFile = outDir ++ "/" ++ "example" ++ ".dot"
  saveGraphViz dotFile labgraph
  putStrLn "Archivo generado!"