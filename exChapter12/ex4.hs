newtype ZipList a = ZipList [a]
                  deriving Show

instance Functor ZipList where
    -- fmap :: (a -> b) -> ZipList a -> ZipList b
    fmap g (ZipList xs) = ZipList (fmap g xs)

instance Applicative ZipList where
    -- pure :: a -> ZipList a
    pure x = ZipList (repeat x)
    -- <*> :: ZipList (a -> b) -> ZipList a -> ZipList b
    ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)  