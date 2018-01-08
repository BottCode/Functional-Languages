data Term = Con Int | Add Term Term
            deriving (Show)

type MOut a = (a,Output)
type Output = String

formatLine :: Term -> Int -> Output
formatLine t a = "eval (" ++ show t ++ ") <= " ++ show a ++ " - "

eval0 :: Term -> MOut Int
eval0 (Con a) = (a,formatLine (Con a) a)
eval0 (Add t u) = ((a + b),(x ++ y ++ formatLine (Add t u) (a + b))) where
    (a, x) = eval0 t
    (b, y) = eval0 u