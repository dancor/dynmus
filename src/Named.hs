module Named where

data Named a = Named
    { getName :: String
    , unName :: a
    }

