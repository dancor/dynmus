module Named where

data Named a = Named
    { getName :: String
    , unName :: a
    }

instance Show a => Show (Named a) where
    show (Named x y) = x ++ ": " ++ show y
