import IncSchemeFuncional.Types
import IncSchemeFuncional.ToDot
import IncSchemeFuncional.Schemes
import IncSchemeFuncional.DenoteSystem 
import IncSchemeFuncional.SetsAndPredicates 


-- | Labeling ad hoc: mapea vértice a nombre lógico
vertexLabel :: [(String, Vertex)] -> Vertex -> String
vertexLabel vmap v =
    case lookup v (map (\(name, vid) -> (vid, name)) vmap) of
        Just name -> name        -- usa el nombre de la variable/expresión
        Nothing   -> "v" ++ show v  -- default para vértices internos

main :: IO ()
main = do
    let example :: System String
        example = schemeToSystem (Inclosure "omega" "x" "delta")

    let refExample = denoteSystem example "omega"
    
        vmap       = buildVertexMap example
        g          = setToGraph refExample
        lab        = vertexLabel vmap

    let labGraph = LabGraph g lab

    putStrLn "DOT:"
    putStrLn (showLabGraphViz labGraph)
    print (refExample)