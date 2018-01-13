{-- Define an instance of the Applicative class for the type (a ->). If you are familiar with combinatory logic, 
    you might recognise pure and <*> for this type as being the well-know K and S combinators --}

instance Applicative ((->) a) where
    -- pure b -> (a -> b)
    pure x = (\_ -> x)  
    
    -- <*> :: f (a -> b) -> f a -> f b 
    -- Our Applicative is a function.  
    f <*> g = (\x -> f x (g x))
    
    