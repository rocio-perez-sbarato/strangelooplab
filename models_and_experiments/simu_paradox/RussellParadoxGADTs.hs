{-# LANGUAGE GADTs #-} 

data False -- Fantasma 
data J c = J (c ()) 

{- Si el conjunto no pertence a si mismo, entonces pertenece al conjunto russelliano R -}
data R :: * -> * where 
   MkR :: (c (J c) -> False) -> R (J c) 

{- La funcion f toma como argumento el mismo R (J R) que construye -}
condFalse :: R (J R) -> False 
condFalse x@(MkR f) = f x 
{-# noinline condFalse #-}

absurd :: False 
absurd = condFalse (MkR condFalse) -- Ciclo de self-reference

main = do
   print (absurd `seq` ())