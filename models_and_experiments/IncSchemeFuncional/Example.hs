module IncSchemeFuncional.Example where 
import IncSchemeFuncional.Types

-- ejemplo: x ⊆ Ω, con δ(x) trascendente y closure definida (sin phi ni psi)
-- Sin tener en cuenta phi ni psi (id)
russellScheme :: Inclosure String 
russellScheme = Inclosure "V" "x" "ρ"