module Chord where

import Data.Function
import Data.List

-- | List of adjacent intervals of a set of distinct pitch classes.
-- Skips one at the end, so a 5-note set of pitch classes lists
-- 4 adjacent interval sizes, measured in half-notes.
--
-- This type is intended to be tonic-agnostic; starting from
-- a different note may give a different list but it should be
-- regarded as the same ChordQual.
type ChordQual = [Int]

-- | This type also represents a set of pitch classes but additionally
-- blesses one as the "tonic" for the set.
--
-- For this type, the intervals are all given in relation to the tonic,
-- in ascending order from the tonic.
--
-- An n-note ChordQual (list of length n - 1) can have up to n different
-- corresponding ModeQuals (or as few as 1, e.g. the whole tone scale).
type ModeQual = [Int]

-- | This places the tonic after the smallest interval possible.
-- For a tie, it prefers big intervals immediately preceding that small one.
--
-- This is a simple algorithm to get leading-tone like behavior from
-- things like the harmonic minor scale or much stranger.
-- Nicely, it also happens to choose the major mode for the major scale.
myChooseMode :: ChordQual -> ModeQual
myChooseMode c = 
    scanl1 (+) . tail $ maximumBy (compare `on` onHeadTail negate reverse) cs
  where 
    cComplete = c ++ [12 - sum c]
    l = length c + 1
    cs = map (take l) $ take l $ tails $ cycle cComplete
    onHeadTail _ _ [] = []
    onHeadTail f g (x:xs) = f x : g xs

hexachords :: [(ChordQual, String)]
hexachords =
    [ ([2,2,2,2,2], "whole tone scale")
    , ([1,3,2,2,2], "9#11")
    , ([1,3,2,1,3], "7b9#11")
    , ([1,3,1,3,2], "m9#11")
    , ([1,3,1,3,1], "augmented scale")
    , ([1,2,3,2,2], "11")
    , ([1,2,3,2,1], "7b9add11")
    , ([1,2,3,1,3], "m7b9#11")
    , ([1,2,3,1,2], "7b5#9add13")
    , ([1,2,2,3,2], "m11")
    , ([1,2,2,3,1], "7#9b13")
    , ([1,2,2,2,3], "m7b9add11")
    , ([1,2,2,2,2], "9b13")
    , ([1,2,2,2,1], "m9b13")
    , ([1,2,2,1,4], "m9add13")
    , ([1,2,2,1,3], "7b9b13")
    , ([1,2,2,1,2], "M11")
    , ([1,2,1,4,2], "m11b5")
    , ([1,2,1,4,1], "M7#9#11")
    , ([1,2,1,3,3], "m7b9b11")
    , ([1,2,1,3,2], "7#9#11")
    , ([1,2,1,3,1], "M7add11b13")
    , ([1,2,1,2,4], "7#11add13")
    , ([1,2,1,2,3], "m7#11add13")
    , ([1,2,1,2,2], "mM11b9")
    , ([1,2,1,2,1], "diminished scale 1-6")
    , ([1,1,5,2,1], "minor scale 1-5 plus 5b")
    , ([1,1,5,1,2], "M11b5")
    , ([1,1,4,3,1], "M#9#11b13")
    , ([1,1,4,2,2], "11b5")
    , ([1,1,4,2,1], "11b5b9")
    , ([1,1,4,1,3], "Mb9#11b13")
    , ([1,1,4,1,2], "mb9#11b13")
    , ([1,1,4,1,1], "chromatic 1-3,7-9")
    , ([1,1,3,4,1], "Mbb7add11b13")
    , ([1,1,3,3,2], "m9b11")
    , ([1,1,3,3,1], "M7b9b13")
    , ([1,1,3,2,3], "M7b5b9add13")
    , ([1,1,3,2,2], "M7b5b9b13")
    , ([1,1,3,2,1], "M7b9#11")
    , ([1,1,3,1,4], "M7#11b13")
    , ([1,1,3,1,3], "mM7#11b13")
    , ([1,1,3,1,2], "M11b9")
    , ([1,1,3,1,1], "M11b5b9")
    , ([1,1,2,5,1], "mM9b5b11")
    , ([1,1,2,4,2], "mM7b9add13")
    , ([1,1,2,4,1], "M11#9")
    , ([1,1,2,3,3], "dim7add9b11")
    , ([1,1,2,3,2], "7#9add11")
    , ([1,1,2,3,1], "mM7b9#11")
    , ([1,1,2,2,4], "7#11b13")
    , ([1,1,2,2,3], "m7#11b13")
    , ([1,1,2,2,2], "mM11b9")
    , ([1,1,2,2,1], "dimM11b9")
    , ([1,1,2,1,5], "minor scale 1-3,5 plus 4b,5b")
    , ([1,1,2,1,4], "M#9add11b13")
    , ([1,1,2,1,3], "mM7b9b11")
    , ([1,1,2,1,2], "major scale 1-5 plus 2b")
    , ([1,1,2,1,1], "chromatic 1-3,5-7")
    , ([1,1,1,6,1], "chromatic 1-2,4-7")
    , ([1,1,1,5,2], "major scale 1-5 plus 5b")
    , ([1,1,1,5,1], "M11b5#9")
    , ([1,1,1,4,3], "minor scale 1,3-6 plus 5b")
    , ([1,1,1,4,2], "11b5#9")
    , ([1,1,1,4,1], "M7sus2b9#11")
    , ([1,1,1,3,4], "M11sus2b9")
    , ([1,1,1,3,3], "minor scale 1-3,6 plus 2b,5b")
    , ([1,1,1,3,2], "major scale 7,1,2,4,5 plus 2b")
    , ([1,1,1,3,1], "harmonic minor 4-8 plus 5b")
    , ([1,1,1,2,5], "major scale 1-5 plus 3b")
    , ([1,1,1,2,4], "Mb5#9add11b13")
    , ([1,1,1,2,3], "major scale 7,1-3,5 plus 2b")
    , ([1,1,1,2,2], "minor scale 1-5 plus 2b")
    , ([1,1,1,2,1], "chromatic 1-4,6,7")
    , ([1,1,1,1,6], "major scale 7,1-4 plus 2b")
    , ([1,1,1,1,5], "chromatic 1,4-8")
    , ([1,1,1,1,4], "chromatic 1-5,9")
    , ([1,1,1,1,3], "chromatic 1-5,8")
    , ([1,1,1,1,2], "chromatic 1-5,7")
    , ([1,1,1,1,1], "chromatic 6")
    ]
