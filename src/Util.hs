module Util where

import Data.Monoid
import qualified Data.Vector as Vec

padr :: Int -> a -> [a] -> [a]
padr n c cs = cs ++ replicate (n - length cs) c

pullEachElem :: Vec.Vector Int -> [(Int, Vec.Vector Int)]
pullEachElem v =
    [ (v Vec.! i, Vec.take i v <> Vec.drop (i + 1) v)
    | i <- [0 .. Vec.length v - 1]
    ]
