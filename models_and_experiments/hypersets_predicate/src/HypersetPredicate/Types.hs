module HypersetPredicate.Types where

import Data.Array 

-- | Sinónimos de tipo básicos 
type Variable = String
type Name = String
type Label = String 
type Vertex = Int
type ID = Int

-- | Expresiones de conjuntos
-- Las funciones se aplican sobre variables, esto se representa mediante Pred 
data SetExpr t
    = SetOf [SetExpr t] 
    | Ref Variable
    | Expr t
    -- Menciona al predicado que se aplica a la variable de la ecuación que lo contiene
    | Pred Name 
    deriving (Eq, Ord, Show) 

-- | Fórmulas de primer orden sobre conjuntos. 
-- En nuestro caso los predicados solo pueden hacer referencias a pertenencia
-- de conjuntos, referenciando a las variables del sistema
data FunApp t
    = SetApp [FunApp t]  
    | Not (FunApp t)
    | Elem (SetExpr t) (SetExpr t)  
    deriving (Eq, Ord, Show)

-- | Un enunciado general: puede ser una ecuación o propiedades de una función.
-- | Construye un sistema de sentencias, 
-- algunas ecuaciones y otras predicados/funciones de valor booleano
data Statement t
    = Function Name (FunApp t)
    | Equation Variable (SetExpr t)
    deriving (Eq, Ord, Show)

-- | Un sistema es una lista de enunciados 
type System t = [Statement t]

-- *************************************

-- Valor lógico extendido
data Value = V0 | V1 | V2
    deriving (Eq, Ord, Show)

{- | Tipo Hereditary Finite Sets con manejo de ciclos 
y planteo esquemático de predicados.  -}
data RefHFScheme t
    = Application Name ID [(t, ID, Value)]       
    | RefS Label ID [RefHFScheme t]       
    | RefU (t, ID)                   
        deriving (Show)

{- | Tipos Graph y LabGraph. Notar que no todas las aristas tienen Label.-}
type Table a = Array Vertex a
type Graph e = Table [(Maybe e, Vertex)] 
type Bounds  = (Vertex, Vertex)
type Edge e = (Vertex, Maybe e, Vertex)

type Labeling a = Vertex -> a
data LabGraph n e = LabGraph (Graph e) (Labeling n)

vertices :: LabGraph n e -> [Vertex]
vertices (LabGraph gr _) = indices gr

labels :: LabGraph n e -> [n]
labels (LabGraph gr l) = map l (indices gr)

-- *************************************
-- | Inclosure Scheme
data Inclosure t = Inclosure
    { omega :: String               -- conjunto Ω "total"
    , x :: String                   -- conjunto x
    , label :: String               -- Función δ 
    }
    deriving (Show)