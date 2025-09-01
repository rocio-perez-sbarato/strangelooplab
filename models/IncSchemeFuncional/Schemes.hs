module IncSchemeFuncional.Schemes where 
import IncSchemeFuncional.Types 

-- ejemplo: x ⊆ Ω, con δ(x) trascendente y closure definida (sin phi ni psi)
schemeToSystem :: Inclosure String -> System String 
schemeToSystem (Inclosure omega x label) =
    [ Equation omega (SetOf [Ref x, Pred label])
    , Equation x     (SetOf [Pred label])
    , Axiom label (And (Transcendence (Ref x) (Ref x) (Expr label)) -- ver qué onda
                            (Closure (Ref x) (Ref omega) (Expr label)))
    ]

-- Borradores

-- Estaba bueno pero hacer una ref a un axiom cambiaba todo
-- schemeToSystem (Inclosure omega x label) =
--     [ Equation omega (SetOf [Expr x])
--     , Equation x     (SetOf [Ref label])
--     , Axiom label (And (Transcendence (Ref x) (Ref omega) (Expr label))
--                             (Closure (Ref x) (Ref omega) (Expr label)))
--     ]


-- Para no repetir tanto δ
-- scheme :: System String
-- scheme =
--     [ Equation "Ω" (SetOf [Pred "δ", Ref "x"])
--     , Equation "x" (SetOf [Pred "δ"]) 
--     , Equation "δ" (SetOf [Fun (\ x -> elem x [x]), Transcendence "x" "x", Closure "x" "Ω", 
--                             Transcendence "Ω" "Ω", Closure "Ω" "Ω"])
--     ]

-- Super separada la función delta
-- scheme =
--     [ Equation "Ω" (SetOf [Expr "x"])
--     , Pred "δ" [Fun (\ x -> elem x [x]), Prop [Transcendence "x" "x", Closure "x" "Ω", 
--                             Transcendence "Ω" "Ω", Closure "Ω" "Ω"]]
--     ]

-- Referencia a la aplicación de una función en ese nodo
-- y lo que implica
-- scheme =
--     [ Equation "Ω" (SetOf [Prop "δ" (\ x -> elem x [x]), Ref "x"])
--     , Equation "x" (SetOf [Prop "δ" (\ x -> elem x [x])]) 
--     , Equation "δ" (SetOf [Transcendence "x" "x", Closure "x" "Ω", 
--                             Transcendence "Ω" "Ω", Closure "Ω" "Ω"])
--     ]

-- Primer borrador
-- [ Equation "Ω" (SetOf [Ref "x", Transcendence "δ" "Ω" "Ω", Closure "δ" "Ω" "Ω"])
-- , Equation "x" (SetOf [Transcendence "δ" "x" "x", Closure "δ" "x" "Ω"])
-- ]