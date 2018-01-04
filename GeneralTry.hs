twice :: (a -> a) -> a -> a
twice f = f.f

myMap :: (a -> a) -> [a] -> [a]
myMap f xs = [f x | x <- xs]

mySecondMap f = foldr ((:) . f) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f xs = [x | x <- xs, f x]

mySecondFilter f [] = []
mySecondFilter f (x:xs)
    | f x = x : mySecondFilter f xs
    | otherwise = mySecondFilter f xs

-- IO tricks --

act :: IO (Char,Char)
act = do x <- getChar
         y <- getChar
         return (x,y) 
