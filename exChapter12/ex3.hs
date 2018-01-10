{-- Define an instance of the Applicative class for the type (a ->). If you are familiar with combinatory logic, 
    you might recognise pure and <*> for this type as being the well-know K and S combinators --}

instance Applicative ((->) r) where
    pure x = (\_ -> x)
    f <*> g = f x (g x)
    