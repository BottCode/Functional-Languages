twice :: (a -> a) -> a -> a
twice f = f.f

myMap :: (a -> a) -> [a] -> [a]
myMap f xs = [f x | x <- xs]