-- A pitch class, stored as a byte ranging from 0 to 11 inclusive.
--
-- To show and read these pitch classes and refer to them by letter,
-- it's recommended to connect with Haskore, e.g.:
--
-- cCl = intToCl $ classToInt Haskore.Basic.Pitch.C

module Cl where

import qualified Data.Set as Set
import Data.Word
import Data.List

data Cl = Cl
    { unCl :: {-# UNPACK #-} !Word8
    } deriving (Eq, Ord)

instance Show Cl where
    show (Cl 0) = "C"
    show (Cl 1) = "C#"
    show (Cl 2) = "D"
    show (Cl 3) = "D#"
    show (Cl 4) = "E"
    show (Cl 5) = "F"
    show (Cl 6) = "F#"
    show (Cl 7) = "G"
    show (Cl 8) = "G#"
    show (Cl 9) = "A"
    show (Cl 10) = "A#"
    show (Cl 11) = "B"
    show _ = error "Show Cl: out of range"

type ClSet = Set.Set Cl

clToInt :: Cl -> Int
clToInt (Cl c) = fromIntegral c

intToCl :: Int -> Cl
intToCl = Cl . fromIntegral . (`mod` 12)

clAdd :: Cl -> Word8 -> Cl
clAdd (Cl a) b = Cl $ (a + b) `mod` 12

-- There is a standard transpositional embedding, so this is legit?
clSum :: Cl -> Cl -> Cl
clSum (Cl a) (Cl b) = Cl $ (a + b) `mod` 12

clDist :: Cl -> Cl -> Int
clDist (Cl a) (Cl b) = min
    ((fromIntegral a - fromIntegral b) `mod` 12)
    ((fromIntegral b - fromIntegral a) `mod` 12)

allCls :: [Cl]
allCls = [Cl i | i <- [0 .. 11]]

{-
clSetDist :: Cl -> ClSet -> Relative
clSetDist c =
    fst . fromJustNote "clToClSetDist" . Set.minView . Set.map (clDist c)

-- The transition difference is how many semitones the notes of the first
-- chord have to move to end up somewhere in the second chord.
clSetTranDist :: ClSet -> ClSet -> Relative
clSetTranDist a b = sum . map (flip clSetDist b) $ Set.toList a

-}

-- The sum of the minimum distances the notes would have to move to remain
-- as six voices. Now: clSetTranDist a b == clSetTranDist b a
clSetTranDist :: ClSet -> ClSet -> Int
clSetTranDist a = minimum . map (sum . zipWith clDist (Set.toList a)) .
    permutations . Set.toList 
