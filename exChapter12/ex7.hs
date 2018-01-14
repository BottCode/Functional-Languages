{--
    Given the following type of expressions
        data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
                    deriving Show
    
    that contain variables of some type a, show how to make this type into instances of the Functor, Applicative and Monad classes.
    With the aid of an example, explain what the >>= operator for this type does
--}

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
            deriving Show

instance Functor Expr where
    -- fmap :: (a -> b) -> Expr a -> Expr b
    fmap _ (Val x) = Val x 
    fmap f (Add lexp rexp) = Expr (fmap f lexp) + fmap f rexp

