-- Here is an example of the main types:
--
-- E3 C4 G4 has the Chord {E3, C4, G4}. This is a set not a list. So exact
--   duplicate notes (like C4 twice) would never appear. The set
--   can be seen as canonically ordered from low to high pitches.
--
-- E3 C4 G4 and C4 E4 G4 both have the same KeyQ. This is the familiar
--   "major" key-quality. It can be represented as 
--   [M3, m3, P4] or [m3, P4, M3] or [M4, M3, m3];
--   that is, the cycle of the pitch class differences starting at any point.
--
-- E3 C4 G4 has the ModeQ [m3, P4, M3]; that is: E to G, G to C, C to E.
--   This fixes "m3" (from the first pitch-class E to the next pitch-class up,
--   which is G here)
--   as the "first" pitch-class difference in the mode-quality.

-- This library mostly tries to describe useful abstractions of note
-- collections. Beyond that, the main prescriptive thing this library does is
-- to decide a canonical mode for any nonempty collection of notes, as the one
-- that has the best
-- "leading tone" behavior ("leading" up to the root note from below),
-- as described in the function keyQToModeQ.

module Chord
  where

import Control.Arrow
import Control.Exception
import Data.Function
import Data.List
import Data.Monoid
import qualified Data.Set as Set
import qualified Data.Vector as Vec

import PiCl
import Named
import Util

type Relative = Int

-- | A chord is a set of notes.
type Chord = Set.Set Int

-- | A key-quality is a cycle (like a cyclical linked list with no designated
-- start point) of pitch-classes differences.
--
-- E.g., the major and harmonic minor modes (which are also 13-chords)
-- have the same key-quality, since they have the same pattern of consecutive
-- pitch-class differences, even though they start at different places in the
-- pattern (major starts with two whole steps, but harmonic minor starts with a
-- whole step then a half step).
--
-- To understand the name "key-quality", consider how the C major scale and
-- harmonic A minor scale are both considered modes of the "key of C major".
-- So a key, like C major, fixes a cycle of adjacent pitch-classes,
-- but doesn't say which pitch-class is "first". The "quality" of
-- the C major key (or the D major key as well) is the "major" key-quality,
-- which fixes a cycle of adjacent pitch-class _differences_, without choosing
-- a "first" pitch-class difference.
--
-- The internal representation is positive adjacent interval differences
-- that add up to 12. This representation doesn't allow us to automatically
-- treat two different start-points of the same cycle as equivalent.
-- Perhaps we should improve this.. normalize the cycle upon creation?
data KeyQ = KeyQ
    { unKeyQ :: !(Vec.Vector Relative)
    } deriving (Eq, Ord)

-- | A mode-quality is a set of pitch-classes one of which is chosen to be
-- "first" (that is, the "tonic" pitch-class).
--
-- E.g., the major and harmonic minor modes (which are also 13-chords)
-- have distinct mode-qualities.
--
-- E.g. normal quality signifiers like "maj", "min7", and "7b9#11"
-- specify mode-qualities. But note that inversions specify additional
-- information, specifying a bass note in addition to a tonic note. If a
-- normal quality signifier like "maj" is used to also indicate not just
-- "major" but further "major in root position", the "root position" part is
-- saying something more specific than a mode-quality.
--
-- The internal representation is positive adjacent interval differences
-- that add up to 12.
data ModeQ = ModeQ
    { unModeQ :: !(Vec.Vector Relative)
    } deriving (Eq, Ord, Show)

-- True Modes should have no repeat PiCls and have the PiCls in an ascending
-- order fitting within some octave.
type Mode = Vec.Vector PiCl

{-
intToClass :: Int -> PiCl
intToClass = go . (`mod` 12)
  where
    go 0 = C
    go 1 = Cs
    go 2 = D
    go 3 = Ds
    go 4 = E
    go 5 = F
    go 6 = Fs
    go 7 = G
    go 8 = Gs
    go 9 = A
    go 10 = As
    go 11 = B
    go _ = error "intToClass: impossible"
-}

modeQAt :: ModeQ -> PiCl -> Mode
modeQAt mq piCl =
    Vec.map intToPiCl . Vec.init . Vec.scanl (+) (piClToInt piCl) $ unModeQ mq

mqFromAdjIvls :: [Relative] -> ModeQ
mqFromAdjIvls rs =
    assert (sum rs == 12 && all (> 0) rs) $
    ModeQ (Vec.fromList rs)

keyQInversions :: KeyQ -> [Vec.Vector Int]
keyQInversions = vRotPoss . unKeyQ

vOrd :: Vec.Vector Int -> Vec.Vector Int
vOrd = Vec.scanl1 (+) . Vec.init

mqInversions :: ModeQ -> [Vec.Vector Int]
mqInversions = map vOrd . vRotPoss . unModeQ

mqOrd :: ModeQ -> Vec.Vector Int
mqOrd (ModeQ v) = vOrd v

-- | This places the tonic after the smallest interval possible.
-- For a tie, it prefers big intervals immediately preceding that small one.
--
-- This is a simple algorithm to get leading-tone like behavior from
-- things like the harmonic minor scale or much stranger.
-- Nicely, it also happens to choose the major mode for the major scale.
--
-- This function returns the inversion-number and the resulting ModeQ. The
-- inversion number is 0 if the ModeQ matches the KeyQ, 1 if the ModeQ starts
-- 1 element in, etc.
keyQToModeQ :: KeyQ -> (Int, ModeQ)
keyQToModeQ =
    second ModeQ . maximumBy (compare `on` negLastRevInit . snd) . zip [0..] . 
    keyQInversions
  where
    negLastRevInit v = Vec.cons (- Vec.last v) (Vec.reverse $ Vec.init v)

vRotPoss :: Vec.Vector a -> [Vec.Vector a]
vRotPoss v =
    map (Vec.fromList . take l) . take l . tails . cycle $ Vec.toList v
  where
    l = Vec.length v

nameMq :: ModeQ -> String
nameMq (ModeQ v) =
    condense . concatMap doLetters . group . Vec.toList $ Vec.init v
  where
    doLetters xs@(x:_) = [consonant, vowel]
      where
        consonant = case x of
          1 -> 't'
          2 -> 'n'
          3 -> 'm'
          4 -> 'r'
          5 -> 'l'
          6 -> 'j'
          7 -> 'k'
          8 -> 'f'
          9 -> 'b'
          10 -> 's'
          11 -> 'd'
          _ -> error "doLetters: consonant"
        vowel = case length xs of
          1 -> 'a'
          2 -> 'e'
          3 -> 'i'
          4 -> 'o'
          5 -> 'u'
          6 -> 'y'
          _ -> error "doLetters: vowel"
    doLetters [] = error "doLetters: []"

    condense [c1, 'a'] = ['a', c1]
    condense ('t':'a':'r':'a':rest) = 't' : condense ('r':'a':rest)
    condense (c1:v1:'n':'a':'t':'a':rest) = c1 : v1 : 'n' : 't' : condense rest
    condense (c1:v1:'r':'a':'t':'a':rest) = c1 : v1 : 'r' : 't' : condense rest
    condense (c1:v1:'r':'a':'n':'a':rest) = c1 : v1 : 'r' : 'n' : condense rest
    condense (c1:v1:'l':'a':'t':'a':rest) = c1 : v1 : 'l' : 't' : condense rest
    condense (c1:v1:'l':'a':'n':'a':rest) = c1 : v1 : 'l' : 'n' : condense rest
    condense (c1:v1:'l':'a':'m':'a':rest) = c1 : v1 : 'l' : 'm' : condense rest
    condense (c1:v1:c2:'a':rest)      = c1 : v1 : c2 : condense rest
    condense (c1:v1:rest)             = c1 : v1 : condense rest
    condense [x] = error $ "condense: parity violation: " ++ [x]
    condense [] = []

genNChords :: Int -> [Named ModeQ]
genNChords n = map (\x -> Named (nameMq x) x) . nub . sort $
    map (snd . keyQToModeQ . KeyQ . Vec.fromList) naive
  where
    naive = concat
        [ map (maxIntvl :) $ grow maxIntvl (n - 1) (12 - maxIntvl)
        | maxIntvl <- [1 .. 12]
        ]
    grow _ 0 0 = [[]]
    grow _ 0 _ = []
    grow maxIntvl notesLeft stonesLeft = if stonesLeft <= 0 then [] else concat
        [ map (intvl :) $ grow maxIntvl (notesLeft - 1) (stonesLeft - intvl)
        | intvl <- [1 .. maxIntvl]
        ]

diffsToAsc :: Vec.Vector Int -> Vec.Vector Int
diffsToAsc = Vec.init . Vec.scanl (+) 0

ascToDiffs :: Vec.Vector Int -> Vec.Vector Int
ascToDiffs v = Vec.zipWith (-) (Vec.snoc (Vec.tail v) (12 + Vec.head v)) v

subchords :: ModeQ -> [ModeQ]
subchords (ModeQ v) =
    map (ModeQ . ascToDiffs . snd) . pullEachElem $ diffsToAsc v

submodes :: Mode -> [Mode]
submodes = map snd . pullEachElem

modeToModeQ :: Mode -> ModeQ
modeToModeQ m = ModeQ $
    Vec.zipWith piClDiff (Vec.tail m <> Vec.singleton (Vec.head m)) m

nameMode :: Mode -> (PiCl, String)
nameMode m = (m Vec.! inversionIndex, nameMq stdMq)
  where
    (inversionIndex, stdMq) = keyQToModeQ $ KeyQ $ unModeQ $ modeToModeQ m

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

nmqAtCl :: Named ModeQ -> Cl -> Named ClSet
nmqAtCl (Named n mq) c = Named (showCl c <> n) . Set.fromList .
    map (Cl . (`mod` 12)) . scanl (+) (unCl c) . map fromIntegral .
    Vec.toList $ unModeQ mq
    -}
