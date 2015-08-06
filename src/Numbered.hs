module Numbered where

data Numbered a = Numbered
    { getNumber :: Int
    , unNumber :: a
    }

instance Show a => Show (Numbered a) where
    show (Numbered n x) = show n ++ ": " ++ show x

onNumbered f (Numbered n x) = Numbered n $ f x
