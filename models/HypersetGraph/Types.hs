module HypersetGraph.Types where

import Data.Array

-- === Tipos de datos básicos ===
type Variable = String
type Label = String 
type Vertex = Int

-- === Tipo System ===
data SetExpr t = Ref Variable | Expr t | SetOf [SetExpr t] deriving (Show)

data Equation t = Equation Variable (SetExpr t) 
  deriving (Show)

-- Ejemplo: [Equation "x" (Expr 5), Equation "y" (SetOf [Ref "x", Expr 10])]
-- Omega: [Equation "X" (SetOf [Ref "X"])]
type System t = [Equation t] 

-- === Tipo HereditaryFiniteSets con manejo de ciclos ===
-- Un ciclo es una referencia circular a un vértice
-- y se maneja con una etiqueta y un vértice.
-- Usa una etiqueta y un vértice para identificar cada elemento
data RefHFS t = RefS Label Vertex [RefHFS t] | RefU (t, Label, Vertex)
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