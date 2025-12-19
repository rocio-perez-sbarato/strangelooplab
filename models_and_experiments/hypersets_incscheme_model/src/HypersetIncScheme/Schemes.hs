module HypersetIncScheme.Schemes where 
import HypersetIncScheme.Types 

-- | Traduce un Inclosure Scheme a un sistema de ecuaciones y predicados/funciones booleanas.
--
--   La clave de la representación es el constructor `Function`, que permite
--   codificar explícitamente las propiedades que la función δ satisface.
--   En particular, se modelan las condiciones de:
--
--   • Closure:     δ(x) ∈ Ω
--   • Transcendence: δ(x) ∉ x
schemeToSystem :: Inclosure String -> System String
schemeToSystem (Inclosure omega x delta) =
    [ Equation omega (SetOf [Ref x])     -- Ω = {x}
    , Equation x (SetOf [Pred delta])
    , Function delta (SetApp [
                            Not (Elem (Ref x) (Ref x)),   -- δ(x) !⋲ x
                            Elem (Ref x) (Ref omega)] )   -- δ(x) ⋲ Ω
    ]                                                                                                                                                   

-- | Caso borde del esquema de Inclosure
schemeBorderCase :: Inclosure String -> System String
schemeBorderCase (Inclosure omega x delta) =
    [ Equation omega (SetOf [Pred delta])    
    , Function delta (SetApp [ -- δ(Ω) ⋲ Ω!
                            Elem (Ref omega) (Ref omega),      
                            Not (Elem (Ref omega) (Ref omega))])  
    ]            