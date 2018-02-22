import Control.Monad.ST

type Stack = [Int]

pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()  
push a = State $ \xs -> ((),a:xs)  