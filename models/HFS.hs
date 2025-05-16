-- herederitary finite sets
data HFS t = U t | S [HFS t] deriving (Eq, Show)

{-# noinline q #-}
q :: HFS String
q = S [U "E", S [q, U "0"]]

-- q = {E, {q, 0}}  donde q se refiere a sí mismo


