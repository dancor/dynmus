-- A pitch class, stored as a byte ranging from 0 to 11 inclusive.

module Cl where

import qualified Data.Set as Set
import Data.Word
import Haskore.Basic.Pitch
import Safe

data Cl = Cl
    { unCl :: {-# UNPACK #-} !Word8
    } deriving (Eq, Ord)

type ClSet = Set.Set Cl

clDist :: Cl -> Cl -> Relative
clDist (Cl a) (Cl b) = min
    ((fromIntegral a - fromIntegral b) `mod` 12)
    ((fromIntegral b - fromIntegral a) `mod` 12)

clSetDist :: Cl -> ClSet -> Relative
clSetDist c =
    fst . fromJustNote "clToClSetDist" . Set.minView . Set.map (clDist c)

-- The transition difference is how many semitones the notes of the first
-- chord have to move to end up somewhere in the second chord.
clSetTranDist :: ClSet -> ClSet -> Relative
clSetTranDist a b = sum . map (flip clSetDist b) $ Set.toList a

clAdd :: Cl -> Word8 -> Cl
clAdd (Cl a) b = Cl $ (a + b) `mod` 12

showCl :: Cl -> String
showCl (Cl 0) = "C"
showCl (Cl 1) = "Db"
showCl (Cl 2) = "D"
showCl (Cl 3) = "Eb"
showCl (Cl 4) = "E"
showCl (Cl 5) = "F"
showCl (Cl 6) = "Gb"
showCl (Cl 7) = "G"
showCl (Cl 8) = "Ab"
showCl (Cl 9) = "A"
showCl (Cl 10) = "Bb"
showCl (Cl 11) = "B"
showCl (Cl i) = error $ "showCl: out of range:" ++ show i

clInt :: Cl -> Int
clInt (Cl c) = fromIntegral c
