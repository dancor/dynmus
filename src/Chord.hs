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
--
-- The Ints will all be positive.
type ChordQual = [Int]

-- | This type also represents a set of pitch classes but additionally
-- blesses one as the "tonic" for the set.
--
-- For this type, the intervals are all given in relation to the tonic,
-- in ascending order from the tonic.
--
-- An n-note ChordQual (list of length n - 1) can have up to n different
-- corresponding ModeQuals (or as few as 1, e.g. the whole tone scale).
--
-- The Ints will all be positive and strictly monotonically increasing.
type ModeQual = [Int]

-- | This places the tonic after the smallest interval possible.
-- For a tie, it prefers big intervals immediately preceding that small one.
--
-- This is a simple algorithm to get leading-tone like behavior from
-- things like the harmonic minor scale or much stranger.
-- Nicely, it also happens to choose the major mode for the major scale.
myChooseMode :: ChordQual -> ModeQual
myChooseMode = scanl1 (+) . tail . myStandardCqc

myStandardCqc :: ChordQual -> ChordQual
myStandardCqc =
    maximumBy (compare `on` onHeadTail negate reverse) .
    cqcRotPoss . cqComplete
  where 
    onHeadTail _ _ [] = []
    onHeadTail f g (x:xs) = f x : g xs

normalizeCq :: ChordQual -> ChordQual
normalizeCq = normalizeCqc . cqComplete

normalizeCqc :: ChordQual -> ChordQual
normalizeCqc = init . minimum . cqcRotPoss

cqComplete :: ChordQual -> [Int]
cqComplete cq = cq ++ [12 - sum cq]

cqcRotPoss :: [Int] -> [[Int]]
cqcRotPoss cqc = map (take l) $ take l $ tails $ cycle cqc
  where
    l = length cqc

trichords :: [(ChordQual, String)]
trichords =
    [ ([4,4], "aug")
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

cManetam, cMano, cMantanam, cManter, cMantman, cMateme, cMatenar      :: [Int]
cMatil, cMatmatam, cMatnatar, cMatnem, cNamani, cNamantam, cNamtanam  :: [Int]
cNamter, cNatelan, cNatemar, cNatenal, cNateram, cNatij, cNatmanam    :: [Int]
cNatmatar, cNatmen, cNatname, cNatnaran, cNatnatal, cNatner, cNatrane :: [Int]
cNemane, cNetel, cNetme, cNetnar, cNetran, cNiman, cNitar, cNom, cNu  :: [Int]
cRantem, cTamanem, cTamantar, cTamene, cTamnaman, cTamtame, cTamtanar :: [Int]
cTamtel, cTaneme, cTaneran, cTanetal, cTanir, cTanmanam, cTanmatar    :: [Int]
cTanmen, cTanrane, cTantej, cTantlan, cTantmar, cTantnal, cTantram    :: [Int]
cTelane, cTemi, cTemnar, cTemran, cTemtal, cTenel, cTenlan, cTenmar   :: [Int]
cTenram, cTentaj, cTerman, cTernam, cTertar, cTijan, cTilam, cTimal   :: [Int]
cTinaj, cTire, cTok, cTrani, cTrantam, cTrater                        :: [Int]

cManetam  = [3,2,2,1,3]
cMano     = [3,2,2,2,2]
cMantanam = [3,2,1,2,3]
cManter   = [3,2,1,1,4]
cMantman  = [3,2,1,3,2]
cMateme   = [3,1,1,3,3]
cMatenar  = [3,1,1,2,4]
cMatil    = [3,1,1,1,5]
cMatmatam = [3,1,3,1,3]
cMatnatar = [3,1,2,1,4]
cMatnem   = [3,1,2,2,3]
cNamani   = [2,3,2,2,2]
cNamantam = [2,3,2,1,3]
cNamtanam = [2,3,1,2,3]
cNamter   = [2,3,1,1,4]
cNatelan  = [2,1,1,5,2]
cNatemar  = [2,1,1,3,4]
cNatenal  = [2,1,1,2,5]
cNateram  = [2,1,1,4,3]
cNatij    = [2,1,1,1,6]
cNatmanam = [2,1,3,2,3]
cNatmatar = [2,1,3,1,4]
cNatmen   = [2,1,3,3,2]
cNatname  = [2,1,2,3,3]
cNatnaran = [2,1,2,4,2]
cNatnatal = [2,1,2,1,5]
cNatner   = [2,1,2,2,4]
cNatrane  = [2,1,4,2,2]
cNemane   = [2,2,3,2,2]
cNetel    = [2,2,1,1,5]
cNetme    = [2,2,1,3,3]
cNetnar   = [2,2,1,2,4]
cNetran   = [2,2,1,4,2]
cNiman    = [2,2,2,3,2]
cNitar    = [2,2,2,1,4]
cNom      = [2,2,2,2,3]
cNu       = [2,2,2,2,2]
cRantem   = [4,2,1,1,3]
cTamanem  = [1,3,2,2,3]
cTamantar = [1,3,2,1,4]
cTamene   = [1,3,3,2,2]
cTamnaman = [1,3,2,3,2]
cTamtame  = [1,3,1,3,3]
cTamtanar = [1,3,1,2,4]
cTamtel   = [1,3,1,1,5]
cTaneme   = [1,2,2,3,3]
cTaneran  = [1,2,2,4,2]
cTanetal  = [1,2,2,1,5]
cTanir    = [1,2,2,2,4]
cTanmanam = [1,2,3,2,3]
cTanmatar = [1,2,3,1,4]
cTanmen   = [1,2,3,3,2]
cTanrane  = [1,2,4,2,2]
cTantej   = [1,2,1,1,6]
cTantlan  = [1,2,1,5,2]
cTantmar  = [1,2,1,3,4]
cTantnal  = [1,2,1,2,5]
cTantram  = [1,2,1,4,3]
cTelane   = [1,1,5,2,2]
cTemi     = [1,1,3,3,3]
cTemnar   = [1,1,3,2,4]
cTemran   = [1,1,3,4,2]
cTemtal   = [1,1,3,1,5]
cTenel    = [1,1,2,2,5]
cTenlan   = [1,1,2,5,2]
cTenmar   = [1,1,2,3,4]
cTenram   = [1,1,2,4,3]
cTentaj   = [1,1,2,1,6]
cTerman   = [1,1,4,3,2]
cTernam   = [1,1,4,2,3]
cTertar   = [1,1,4,1,4]
cTijan    = [1,1,1,6,2]
cTilam    = [1,1,1,5,3]
cTimal    = [1,1,1,3,5]
cTinaj    = [1,1,1,2,6]
cTire     = [1,1,1,4,4]
cTok      = [1,1,1,1,7]
cTrani    = [1,4,2,2,2]
cTrantam  = [1,4,2,1,3]
cTrater   = [1,4,1,1,4]

namedHexachords :: [([Int], String)]
namedHexachords =
    [ (cManetam,  "manetam")  -- 1
    , (cMano,     "mano")
    , (cMantanam, "mantanam")
    , (cManter,   "manter")
    , (cMantman,  "mantman")
    , (cMateme,   "mateme")   -- 6
    , (cMatenar,  "matenar")
    , (cMatil,    "matil")
    , (cMatmatam, "matmatam")
    , (cMatnatar, "matnatar")
    , (cMatnem,   "matnem")   -- 11
    , (cNamani,   "namani")
    , (cNamantam, "namantam")
    , (cNamtanam, "namtanam")
    , (cNamter,   "namter")
    , (cNatelan,  "natelan")  -- 16
    , (cNatemar,  "natemar")
    , (cNatenal,  "natenal")
    , (cNateram,  "nateram")
    , (cNatij,    "natij")
    , (cNatmanam, "natmanam") -- 21
    , (cNatmatar, "natmatar")
    , (cNatmen,   "natmen")
    , (cNatname,  "natname")
    , (cNatnaran, "natnaran")
    , (cNatnatal, "natnatal") -- 26
    , (cNatner,   "natner")
    , (cNatrane,  "natrane")
    , (cNemane,   "nemane")
    , (cNetel,    "netel")
    , (cNetme,    "netme")    -- 31
    , (cNetnar,   "netnar")
    , (cNetran,   "netran")
    , (cNiman,    "niman")
    , (cNitar,    "nitar")
    , (cNom,      "nom")      -- 36
    , (cNu,       "nu")
    , (cRantem,   "rantem")
    , (cTamanem,  "tamanem")
    , (cTamantar, "tamantar")
    , (cTamene,   "tamene")   -- 41
    , (cTamnaman, "tamnaman")
    , (cTamtame,  "tamtame")
    , (cTamtanar, "tamtanar")
    , (cTamtel,   "tamtel")
    , (cTaneme,   "taneme")   -- 46
    , (cTaneran,  "taneran")
    , (cTanetal,  "tanetal")
    , (cTanir,    "tanir")
    , (cTanmanam, "tanmanam")
    , (cTanmatar, "tanmatar") -- 51
    , (cTanmen,   "tanmen")
    , (cTanrane,  "tanrane")
    , (cTantej,   "tantej")
    , (cTantlan,  "tantlan")
    , (cTantmar,  "tantmar")  -- 56
    , (cTantnal,  "tantnal")
    , (cTantram,  "tantram")
    , (cTelane,   "telane")
    , (cTemi,     "temi")
    , (cTemnar,   "temnar")   -- 61
    , (cTemran,   "temran")
    , (cTemtal,   "temtal")
    , (cTenel,    "tenel")
    , (cTenlan,   "tenlan")
    , (cTenmar,   "tenmar")   -- 66
    , (cTenram,   "tenram")
    , (cTentaj,   "tentaj")
    , (cTerman,   "terman")
    , (cTernam,   "ternam")
    , (cTertar,   "tertar")   -- 71
    , (cTijan,    "tijan")
    , (cTilam,    "tilam")
    , (cTimal,    "timal")
    , (cTinaj,    "tinaj")
    , (cTire,     "tire")     -- 76
    , (cTok,      "tok")
    , (cTrani,    "trani")
    , (cTrantam,  "trantam")
    , (cTrater,   "trater")   -- 80
    ]
