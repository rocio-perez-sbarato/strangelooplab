module Elements where

import Data.Array ( Array )

type Vertex = Int
type Table a = Array Vertex a
type Graph e = Table [(e, Vertex)]
type Bounds  = (Vertex, Vertex)
type Edge e = (Vertex, e, Vertex)

type Labeling a = Vertex -> a
data LabGraph n e = LabGraph (Graph e) (Labeling n)

type ID = Int
data RefHFS t = RefS ID [RefHFS t] | RefU ID 
    deriving (Show)

{- | Representamos predicados y funciones como conjuntos de tipo String. 
Esto es porque la implementación es sintáctica y la semántica se ve 
reflejada en el grafo resultante. 
-}
type Funcion = String
type Elemento = String -- Nodos

{- | Representamos los conjuntos como conjuntos hereditarios finitos para lograr una fácil construcción del grafo.
En caso de subconjuntos optamos por dirigirnos a ellos como Strings, 
ya que estos grafos no avanzarán en profundidad más allá del nivel 2. 
-}

-- | Representación del Inclosure Schema
data Inclosure = Inclosure
    { omega    :: Elemento
    , x :: Elemento
    , epsi   :: Funcion
    , phi  :: Funcion
    , delta     :: Funcion
    }
