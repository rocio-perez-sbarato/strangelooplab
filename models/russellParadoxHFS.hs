-- Tipo de conjunto hereditarimente finito con soporte para ciclos
data HFS t = U t | S [HFS t] deriving (Eq, Show)

-- Función para saber si un conjunto está en otro 
elemHFS :: Eq t => HFS t -> HFS t -> Bool
elemHFS x (S xs) = x `elem` xs
elemHFS _ _      = False

-- Creamos un elemento para el conjunto R
x :: HFS String
x = S [] -- No se contiene a sí mismo

-- Proponemos que R no se contiene a sí mismo
r :: HFS String
r = S [ y | y <- [x, r], not (y `elemHFS` y) ]

main :: IO ()
main = do
    print ((r `elemHFS` r) `seq` ()) -- Ciclo de self-reference
