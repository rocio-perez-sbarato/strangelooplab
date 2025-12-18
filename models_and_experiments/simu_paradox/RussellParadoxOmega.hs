data False

-- Conjunto ruselliano
data R = MkR {proj :: R -> False}
    
-- R pertenece a si mismo?
f :: R -> False
f = \x -> proj x x
-- La clave: evitar optimizaciones de GHC
{-# noinline f #-} 
    
omega :: False
omega = f (MkR f) -- Ciclo de self-reference
    
main = do
    print (omega `seq` ())