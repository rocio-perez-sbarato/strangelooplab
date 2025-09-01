module Elements where

import Data.Array ( Array )

{- | Representamos el grafo como un arreglo de aristas indexados por vértices. 
La clave es que las aristas tienen una label. 
-}
type Vertex = Int
type Table a = Array Vertex a
type Graph e = Table [(e, Vertex)]
type Bounds  = (Vertex, Vertex)
type Edge e = (Vertex, e, Vertex)

type Labeling a = Vertex -> a
data LabGraph n e = LabGraph (Graph e) (Labeling n)

{- | Representamos predicados y funciones como conjuntos de tipo String. 
Esto es porque la implementación es sintáctica y la semántica se ve 
reflejada en el grafo resultante. 
-}
type Funcion = String
type Elemento = String -- Nodos

{- | Representación del Inclosure Schema. 
El orden de los parámetros corresponde al planteo del Inclosure Scheme. 
-}
data Inclosure = Inclosure
    { omega    :: Elemento
    , x :: Elemento
    , epsi   :: Funcion
    , phi  :: Funcion
    , delta     :: Funcion
    }
