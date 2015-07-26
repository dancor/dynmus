module Named where

data Named a = Named
    { getName :: String
    , unName :: a
    }

instance Show a => Show (Named a) where
    show (Named n x) = n ++ ": " ++ show x

onNamed f (Named n x) = Named n $ f x
