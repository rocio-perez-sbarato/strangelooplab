{-|
Module      : Hyperset.Types
Description : Tipos principales para representar sistemas de ecuaciones, conjuntos y grafos en ZFA
Copyright   : (c) Rocío Perez Sbarato, 2025
License     : MIT
Maintainer  : rocio.perez.sbarato@mi.unc.edu.ar
Stability   : experimental
Portability : portable
-}

module Hyperset.Types where
import Data.Array ( Array )

-- | Tipos de datos básicos 
type Variable = String
type Label = String 
type Vertex = Int
type ID = Int

-- | Tipo System
data SetExpr t = Ref Variable | Expr t | SetOf [SetExpr t] deriving (Show)
data Equation t = Equation Variable (SetExpr t) 
  deriving (Show)

type System t = [Equation t] 

-- | Tipo Hereditary Finite Sets con manejo de ciclos
data RefHFS t = RefS ID [RefHFS t] | RefU (t, ID) 
  deriving (Show)

-- | Tipo Hereditary Finite Sets 
data HFS t = S [HFS t] | U t 
  deriving (Eq, Show)

{- | Tipos Graph y LabGraph. Notar que Labeling es
de cada vértice es un conjunto HFS.-}

type Graph = Array Vertex [Vertex]
type Edge = (Vertex, Vertex)
type Bounds = (Vertex, Vertex)

type Labeling a = Vertex -> HFS a 
data LabGraph n = LabGraph Graph (Labeling n) 