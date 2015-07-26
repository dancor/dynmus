module Chord
{-
  ( Chord
  , ChordQ
  , cqInversions
  , ltChooseMode
  , ModeQ (..)
  , mqOrd
  , mqFromAdjIvls
  , mqInversions
  , normalizeCq
  , vRotPoss
  )
  -}
  where

import Control.Exception
import Data.Function
import Data.List
import qualified Data.Set as Set
import Haskore.Basic.Pitch
import qualified Data.Vector as Vec

--import Named

-- | A chord is a set of notes.
type Chord = Set.Set Int

-- | A chord-quality is a set of pitch-classes.
--
-- E.g., the major and harmonic minor modes (which are also 13-chords)
-- have the same chord-quality.
--
-- E.g., "maj" (like C-E-G) and "b3#5" ("augmented minor"?)
-- (like E-G-B# seen with an E tonic instead of a C tonic 1st inversion)
-- have the same chord-quality.
--
-- The internal representation is positive adjacent interval differences
-- that add up to 12.
data ChordQ = ChordQ
    { unCQ :: !(Vec.Vector Relative)
    }

-- | A mode-quality is a set of pitch-classes one of which is chosen to be
-- "first" (that is, the "tonic" pitch-class).
--
-- E.g., the major and harmonic minor modes (which are also 13-chords)
-- have distinct mode-qualities.
--
-- E.g. normal chord signifiers like "maj", "min7", and "7b9#11"
-- specify mode-qualities. But note that inversions specify addition
-- information, specifying a bass note in addition to a tonic note.
-- Since normal chord signifiers may signify root position chords or not,
-- there is an ambiguity.
--
-- The internal representation is positive adjacent interval differences
-- that add up to 12.
data ModeQ = ModeQ
    { unMQ :: !(Vec.Vector Relative)
    }

type Mode = Vec.Vector Relative

formMode :: Relative -> ModeQ -> Mode
formMode n = Vec.map ((`mod` 12) . (n +)) . unMQ

mqFromAdjIvls :: [Relative] -> ModeQ
mqFromAdjIvls rs =
    assert (sum rs == 12 && all (> 0) rs && all (< 12) rs) $
    ModeQ (Vec.fromList rs)

cqInversions :: ChordQ -> [Vec.Vector Int]
cqInversions = vRotPoss . unCQ

vOrd :: Vec.Vector Int -> Vec.Vector Int
vOrd = Vec.scanl1 (+) . Vec.init

mqInversions :: ModeQ -> [Vec.Vector Int]
mqInversions = map vOrd . vRotPoss . unMQ

mqOrd :: ModeQ -> Vec.Vector Int
mqOrd (ModeQ v) = vOrd v

-- | This places the tonic after the smallest interval possible.
-- For a tie, it prefers big intervals immediately preceding that small one.
--
-- This is a simple algorithm to get leading-tone like behavior from
-- things like the harmonic minor scale or much stranger.
-- Nicely, it also happens to choose the major mode for the major scale.
ltChooseMode :: ChordQ -> ModeQ
ltChooseMode =
    ModeQ . maximumBy (compare `on` negHeadRevRest) . cqInversions
  where
    negHeadRevRest v = Vec.cons (- Vec.head v) (Vec.reverse $ Vec.tail v)

normalizeCq :: ChordQ -> ChordQ
normalizeCq = ChordQ . minimum . cqInversions

vRotPoss :: Vec.Vector a -> [Vec.Vector a]
vRotPoss v =
    map (Vec.fromList . take l) . take l . tails . cycle $ Vec.toList v
  where
    l = Vec.length v

{-
trichords :: [Named ModeQ]
trichords =
    [ Named "aug" $ mqFromAdjInts [4,4]
    , ([3,5], "M")
    , ([3,4], "m")
    , ([3,3], "dim")
    , ([2,7], "7no3")
    , ([2,6], "m7b5no3")
    , ([2,5], "sus4")
    , ([2,4], "7no5 (Italian 6th)")
    , ([2,3], "m7no5")
    , ([2,2], "major scale 1-3")
    , ([1,9], "minor scale 1-3")
    , ([1,8], "augM7no3")
    , ([1,7], "M7no3")
    , ([1,6], "dimM7no3")
    , ([1,5], "sus#4")
    , ([1,4], "M7no5")
    , ([1,3], "mM7no5")
    , ([1,2], "major scale 7-2")
    , ([1,1], "chromatic 3")
    ]
    -}
