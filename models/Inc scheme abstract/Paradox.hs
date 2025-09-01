module Paradox where

import Elements 

-- Inclosure Schema instanciado
genInclosure :: Inclosure
genInclosure = Inclosure "Ω" "x" "ψ" "ϕ" "δ"

-- Caso especial de Inclosure Scheme para Russell Scheme
rusellScheme :: Inclosure
rusellScheme = Inclosure "V" "x" "id" "⋲ V" "ρ"