module Hyperset.Types where

import Data.Array

-- === Tipos de datos básicos ===
type Variable = String
type Label = String 
type Vertex = Int
type ID = Int

-- === Tipo System ===
data SetExpr t = Ref Variable | Expr t | SetOf [SetExpr t] deriving (Show)

data Equation t = Equation Variable (SetExpr t) 
  deriving (Show)

type System t = [Equation t] 

-- === Tipo HereditaryFiniteSets con manejo de ciclos ===
-- Tiene info extra sobre Variables, para simplificar la estructura podría borrarlas
data RefHFS t = RefS Variable ID [RefHFS t] | RefU (t, Variable, ID) 
  deriving (Show)

-- === Tipo HereditaryFiniteSets ===
data HFS t = S [HFS t] | U t 
  deriving (Eq, Show)

-- === Tipos Graph y LabGraph ===
type Graph = Array Vertex [Vertex]
type Edge = (Vertex, Vertex)
type Bounds = (Vertex, Vertex)

type Labeling a = Vertex -> HFS a 
data LabGraph n = LabGraph Graph (Labeling n) 