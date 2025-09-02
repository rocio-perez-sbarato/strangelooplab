{-|
Module      : Hyperset.Types
Description : Tipos principales para representar sistemas de ecuaciones, conjuntos y grafos en ZFA
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module IncSchemeFuncional.Types where

import Data.Array 
import Text.Show.Functions ()

-- | Tipos de datos básicos 
type Variable = String
type Name = String
type Label = String 
type Vertex = Int
type ID = Int

-- Valor lógico extendido
data Value = V0 | V1 | V2
    deriving (Eq, Ord, Show)

-- | Expresiones de conjuntos (los "términos")
data SetExpr t
    = Ref Variable
    | Expr t
    | SetOf [SetExpr t]
    | Pred Name
    deriving (Eq, Ord, Show)

-- | Fórmulas de primer orden sobre conjuntos
data FunProp t
    = SetProp [FunProp t]
    | Not   (FunProp t)
    | And   (FunProp t) (FunProp t)
    | Elem  (SetExpr t) (SetExpr t)     -- pertenencia entre elementos
    deriving (Eq, Ord, Show)

-- | Un enunciado general: puede ser una "ecuación" o un "axioma"
data Statement t
    = Axiom Name (FunProp t)
    | Equation Variable (SetExpr t)
    deriving (Eq, Ord, Show)

-- | Un sistema es una lista de enunciados
type System t = [Statement t]

-- *************************************

{- | Tipo Hereditary Finite Sets con manejo de ciclos 
y planteo esquemático de dominio e imagen de funciones 
-}

-- Apply from [name, to, value]
-- Por el momento, value no hace nada. Habría que evaluarlo
data RefHFScheme t
    = Apply [(t, ID, Value)]   -- función aplicada a argumentos
    | RefS ID [RefHFScheme t]    -- conjuntos
    | RefU (t, ID)               -- átomos
    deriving (Eq, Ord, Show)

-- | Tipo Hereditary Finite Sets 
data HFS t = S [HFS t] | U t 
    deriving (Eq, Show)

{- | Tipos Graph y LabGraph. Notar que Labeling es
de cada vértice es un conjunto HFS.-}

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
    { omega :: String               -- label para conjunto Ω “total”
    , x :: String                   -- label para subconjunto x
    , label :: String               -- label para δ 
    }
    deriving (Show)
