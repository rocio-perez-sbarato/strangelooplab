{-# LANGUAGE GADTs, KindSignatures, EmptyDataDecls #-} 

data False -- Fantasma 
data J c = J (c ()) 

{- Si el conjunto no pertence a si mismo, entonces pertenece al conjunto russelliano R -}
data R :: * -> * where 
   MkR :: (c (J c) -> False) -> R (J c) 

{- La funcion f toma como argumento el mismo R (J R) que construye -}
cond_false :: R (J R) -> False 
cond_false x@(MkR f) = f x 
{-# noinline cond_false #-}

absurd :: False 
absurd = cond_false (MkR cond_false) -- Ciclo de self-reference

main = do
  print (absurd `seq` ())