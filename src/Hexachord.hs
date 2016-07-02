module Hexachord where

import Haskore.Basic.Pitch

import Chord
import Named
import Numbered

type Nnmq = Numbered (Named ModeQ)

allHexachords :: [Nnmq]
allHexachords = [
    hNu, hNemne, hNamni, hNiman, hMano, hNom, hMantman, hNamtanam, hMantnam,
    hNamnatam, hNatmanam, hManetam, hMatnem, hNatmen, hNatname, hTamnaman,
    hTanmanam, hNetme, hTamnem, hTanmen, hTamene, hTaneme, hNatnarn, hNatner,
    hNatrane, hNetnar, hNetran, hTanern, hTanrane, hNitar, hTanir, hTrani,
    hMatmatam, hMateme, hTamtame, hTemi, hMatnatar, hNatmatar, hManter,
    hNamter, hTamnatar, hTanmatar, hMatenar, cRantem, hTamtanar, hTrantam,
    hNatemar, hNateram, hTantmar, hTantram, hTemnar, hTernam, hTenmar, hTerman,
    hTemran, hTenram, hNatnatl, hNetel, hTanetal, hNateln, hNatenal, hTantlan,
    hTantnal, hTenlan, hTelne, hTenel, hTrater, hTamtel, hTantej, hTertar,
    hMatil, hTemtal, hNatij, hTentaj, hTire, hTilm, hTimal, hTijan, hTinaj,
    hTok]

mq6 :: Int -> String
    -> Relative -> Relative -> Relative -> Relative -> Relative -> Relative
    -> Nnmq
mq6 n s a b c d e f = Numbered n . Named s $ mqFromAdjIvls [a, b, c, d, e, f]

hNu, hNemne, hNamni, hNiman, hMano, hNom, hMantman, hNamtanam, hMantnam :: Nnmq
hNamnatam, hNatmanam, hManetam, hMatnem, hNatmen, hNatname, hTamnaman   :: Nnmq
hTanmanam, hNetme, hTamnem, hTanmen, hTamene, hTaneme, hNatnarn         :: Nnmq
hNatner, hNatrane, hNetnar, hNetran, hTanern, hTanrane, hNitar, hTanir  :: Nnmq
hTrani, hMatmatam, hMateme, hTamtame, hTemi, hMatnatar, hNatmatar       :: Nnmq
hManter, hNamter, hTamnatar, hTanmatar, hMatenar, cRantem, hTamtanar    :: Nnmq
hTrantam, hNatemar, hNateram, hTantmar, hTantram, hTemnar, hTernam      :: Nnmq
hTenmar, hTerman, hTemran, hTenram, hNatnatl, hNetel, hTanetal, hNateln :: Nnmq
hNatenal, hTantlan, hTantnal, hTenlan, hTelne, hTenel, hTrater, hTamtel :: Nnmq
hTantej, hTertar, hMatil, hTemtal, hNatij, hTentaj, hTire, hTilm        :: Nnmq
hTimal, hTijan, hTinaj, hTok                                            :: Nnmq                         
-- 80 hexachords. Number with n minor-2nds:
-- 0:  1
-- 1:  5
-- 2: 26
-- 3: 34
-- 4: 13
-- 5:  1

-- - Zero 1's:
hNu = mq6 0 "nu" 2 2 2 2 2 2

-- - One 1:
-- - No adjacent triples:
hNemne = mq6 1 "nemne" 2 2 3 2 2 1
-- - No adjacent quadruples:
hNamni = mq6 2 "namni" 2 3 2 2 2 1
hNiman = mq6 3 "niman" 2 2 2 3 2 1
-- - Adjacent quadruples:
hMano  = mq6 4 "mano"  3 2 2 2 2 1
hNom   = mq6 5 "nom"   2 2 2 2 3 1

-- Two 1's, nothing > 3:
-- - No single-separated or adjacent pairs:
hMantman  = mq6  6 "mantman"  3 2 1 3 2 1
hNamtanam = mq6  7 "namtanam" 2 3 1 2 3 1
-- - No adjacent pairs:
hMantnam  = mq6  8 "mantnam"  3 2 1 2 3 1
hNamnatam = mq6  9 "namnatam" 2 3 2 1 3 1
hNatmanam = mq6 10 "natmanam" 2 1 3 2 3 1
-- - One adjacent pair:
hManetam  = mq6 11 "manetam"  3 2 2 1 3 1
hMatnem   = mq6 12 "mantem"   3 1 2 2 3 1
hNatmen   = mq6 13 "natmen"   2 1 3 3 2 1
hNatname  = mq6 14 "natname"  2 1 2 3 3 1
hTamnaman = mq6 15 "tamnaman" 1 3 2 3 2 1
hTanmanam = mq6 16 "tanmanam" 1 2 3 2 3 1
-- - Two adjacent pairs:
hNetme    = mq6 17 "netme"    2 2 1 3 3 1
hTamnem   = mq6 18 "tamnem"   1 3 2 2 3 1
hTanmen   = mq6 19 "tanmen"   1 2 3 3 2 1
-- - Three adjacent pairs:
hTamene   = mq6 20 "tamene"   1 3 3 2 2 1
hTaneme   = mq6 21 "taneme"   1 2 2 3 3 1

-- Two 1's rest:
-- - No adjacent pairs:
hNatnarn = mq6 22 "natnarn" 2 1 2 4 2 1
-- - One adjacent pair:
hNatner  = mq6 23 "natner"  2 1 2 2 4 1
hNatrane = mq6 24 "natrane" 2 1 4 2 2 1
hNetnar  = mq6 25 "netnar"  2 2 1 2 4 1
hNetran  = mq6 26 "netran"  2 2 1 4 2 1
hTanern  = mq6 27 "tanern"  1 2 2 4 2 1
hTanrane = mq6 28 "tanrane" 1 2 4 2 2 1
-- - Adjacent triple:
hNitar   = mq6 29 "nitar"   2 2 2 1 4 1
hTanir   = mq6 30 "tanir"   1 2 2 2 4 1
hTrani   = mq6 31 "trani"   1 4 2 2 2 1

-- Three 1's, nothing > 3:
-- No pairs:
hMatmatam = mq6 32 "matmatam" 3 1 3 1 3 1
-- Pairs:
hMateme   = mq6 33 "mateme"   3 1 1 3 3 1
hTamtame  = mq6 34 "tamtame"  1 3 1 3 3 1
-- Triples:
hTemi     = mq6 35 "temi"     1 1 3 3 3 1

-- Three 1's, nothing > 4:
-- - No pairs:
hMatnatar = mq6 36 "matnatar" 3 1 2 1 4 1
hNatmatar = mq6 37 "natmatar" 2 1 3 1 4 1
-- - No triples (implies one pair):
-- . - Highest isolated from other highs:
hManter   = mq6 38 "manter"   3 2 1 1 4 1
hNamter   = mq6 39 "namter"   2 3 1 1 4 1
hTamnatar = mq6 40 "tamnatar" 1 3 2 1 4 1
hTanmatar = mq6 41 "tanmatar" 1 2 3 1 4 1
-- . - 2nd highest isolated from other highs:
hMatenar  = mq6 42 "matenar"  3 1 1 2 4 1
cRantem   = mq6 43 "rantem"   4 2 1 1 3 1
hTamtanar = mq6 44 "tamtanar" 1 3 1 2 4 1
hTrantam  = mq6 45 "trantam"  1 4 2 1 3 1
-- . - Rest:
hNatemar  = mq6 46 "natemar"  2 1 1 3 4 1
hNateram  = mq6 47 "nateram"  2 1 1 4 3 1
hTantmar  = mq6 48 "tantmar"  1 2 1 3 4 1
hTantram  = mq6 49 "tantram"  1 2 1 4 3 1
-- - Triples:
-- . - Outliers separated:
hTemnar   = mq6 50 "temnar"   1 1 3 2 4 1
hTernam   = mq6 51 "ternam"   1 1 4 2 3 1
-- . - High near low:
hTenmar   = mq6 52 "tenmar"   1 1 2 3 4 1
hTerman   = mq6 53 "terman"   1 1 4 3 2 1
-- . - Rest:
hTemran   = mq6 54 "temran"   1 1 3 4 2 1
hTenram   = mq6 55 "tenram"   1 1 2 4 3 1

-- Three 1's rest:
-- - No pairs:
hNatnatl = mq6 56 "natnatl" 2 1 2 1 5 1
-- - Pairs:
-- - - High separated:
hNetel   = mq6 57 "netel"   2 2 1 1 5 1
hTanetal = mq6 58 "tanetal" 1 2 2 1 5 1
-- - - Rest:
hNateln  = mq6 59 "nateln"  2 1 1 5 2 1
hNatenal = mq6 60 "natenal" 2 1 1 2 5 1
hTantlan = mq6 61 "tantlan" 1 2 1 5 2 1
hTantnal = mq6 62 "tantnal" 1 2 1 2 5 1
-- - Triples:
-- - - No pair outside triple:
hTenlan  = mq6 63 "tenlan"  1 1 2 5 2 1
-- - - Rest:
hTelne   = mq6 64 "telne"   1 1 5 2 2 1
hTenel   = mq6 65 "tenel"   1 1 2 2 5 1

-- Four 1's:
-- - No triple:
-- . - Uniform outliers:
hTrater = mq6 66 "trater" 1 4 1 1 4 1
-- . - Medium uniform outliers:
hTamtel = mq6 67 "tamtel" 1 3 1 1 5 1
-- . - Rest:
hTantej = mq6 68 "tantej" 1 2 1 1 6 1
-- - No quadruple:
-- . - Uniform outliers:
hTertar = mq6 69 "tertar" 1 1 4 1 4 1
-- . - Medium uniform outliers:
hMatil  = mq6 70 "matil"  3 1 1 1 5 1
hTemtal = mq6 71 "temtal" 1 1 3 1 5 1
-- . - Rest:
hNatij  = mq6 72 "natij"  2 1 1 1 6 1
hTentaj = mq6 73 "tentaj" 1 1 2 1 6 1
-- - Rest:
-- . - Uniform outliers:
hTire   = mq6 74 "tire"   1 1 1 4 4 1
-- . - Medium uniform outliers:
hTilm   = mq6 75 "tilm"   1 1 1 5 3 1
hTimal  = mq6 76 "timal"  1 1 1 3 5 1
-- . - Rest:
hTijan  = mq6 77 "tijan"  1 1 1 6 2 1
hTinaj  = mq6 78 "tinaj"  1 1 1 2 6 1

-- Rest:
hTok = mq6 79 "tok" 1 1 1 1 7 1
