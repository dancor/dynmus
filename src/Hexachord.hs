module Hexachord where

import Haskore.Basic.Pitch

import Chord
import Named
import Numbered

type Nnmq = Numbered (Named ModeQ)

hexachords :: [Nnmq]
hexachords = [
    cNu, cNemne, cNamni, cNiman, cMano, cNom, cMantman, cNamtanam, cMantnam,
    cNamnatam, cNatmanam, cManetam, cMatnem, cNatmen, cNatname, cTamnaman,
    cTanmanam, cNetme, cTamnem, cTanmen, cTamene, cTaneme, cNatnarn, cNatner,
    cNatrane, cNetnar, cNetran, cTanern, cTanrane, cNitar, cTanir, cTrani,
    cMatmatam, cMateme, cTamtame, cTemi, cMatnatar, cNatmatar, cManter,
    cNamter, cTamnatar, cTanmatar, cMatenar, cRantem, cTamtanar, cTrantam,
    cNatemar, cNateram, cTantmar, cTantram, cTemnar, cTernam, cTenmar, cTerman,
    cTemran, cTenram, cNatnatl, cNetel, cTanetal, cNateln, cNatenal, cTantlan,
    cTantnal, cTenlan, cTelne, cTenel, cTrater, cTamtel, cTantej, cTertar,
    cMatil, cTemtal, cNatij, cTentaj, cTire, cTilm, cTimal, cTijan, cTinaj,
    cTok]

mq6 :: Int -> String
    -> Relative -> Relative -> Relative -> Relative -> Relative -> Relative
    -> Nnmq
mq6 n s a b c d e f = Numbered n . Named s $ mqFromAdjIvls [a, b, c, d, e, f]

cNu, cNemne, cNamni, cNiman, cMano, cNom, cMantman, cNamtanam, cMantnam :: Nnmq
cNamnatam, cNatmanam, cManetam, cMatnem, cNatmen, cNatname, cTamnaman   :: Nnmq
cTanmanam, cNetme, cTamnem, cTanmen, cTamene, cTaneme, cNatnarn         :: Nnmq
cNatner, cNatrane, cNetnar, cNetran, cTanern, cTanrane, cNitar, cTanir  :: Nnmq
cTrani, cMatmatam, cMateme, cTamtame, cTemi, cMatnatar, cNatmatar       :: Nnmq
cManter, cNamter, cTamnatar, cTanmatar, cMatenar, cRantem, cTamtanar    :: Nnmq
cTrantam, cNatemar, cNateram, cTantmar, cTantram, cTemnar, cTernam      :: Nnmq
cTenmar, cTerman, cTemran, cTenram, cNatnatl, cNetel, cTanetal, cNateln :: Nnmq
cNatenal, cTantlan, cTantnal, cTenlan, cTelne, cTenel, cTrater, cTamtel :: Nnmq
cTantej, cTertar, cMatil, cTemtal, cNatij, cTentaj, cTire, cTilm        :: Nnmq
cTimal, cTijan, cTinaj, cTok                                            :: Nnmq                         
-- 80 hexachords. Number with n minor-2nds:
-- 0:  1
-- 1:  5
-- 2: 26
-- 3: 34
-- 4: 13
-- 5:  1

-- - Zero 1's:
cNu = mq6 0 "nu" 2 2 2 2 2 2

-- - One 1:
-- - No adjacent triples:
cNemne = mq6 1 "nemne" 2 2 3 2 2 1
-- - No adjacent quadruples:
cNamni = mq6 2 "namni" 2 3 2 2 2 1
cNiman = mq6 3 "niman" 2 2 2 3 2 1
-- - Adjacent quadruples:
cMano  = mq6 4 "mano"  3 2 2 2 2 1
cNom   = mq6 5 "nom"   2 2 2 2 3 1

-- Two 1's, nothing > 3:
-- - No single-separated or adjacent pairs:
cMantman  = mq6  6 "mantman"  3 2 1 3 2 1
cNamtanam = mq6  7 "namtanam" 2 3 1 2 3 1
-- - No adjacent pairs:
cMantnam  = mq6  8 "mantnam"  3 2 1 2 3 1
cNamnatam = mq6  9 "namnatam" 2 3 2 1 3 1
cNatmanam = mq6 10 "natmanam" 2 1 3 2 3 1
-- - One adjacent pair:
cManetam  = mq6 11 "manetam"  3 2 2 1 3 1
cMatnem   = mq6 12 "mantem"   3 1 2 2 3 1
cNatmen   = mq6 13 "natmen"   2 1 3 3 2 1
cNatname  = mq6 14 "natname"  2 1 2 3 3 1
cTamnaman = mq6 15 "tamnaman" 1 3 2 3 2 1
cTanmanam = mq6 16 "tanmanam" 1 2 3 2 3 1
-- - Two adjacent pairs:
cNetme    = mq6 17 "netme"    2 2 1 3 3 1
cTamnem   = mq6 18 "tamnem"   1 3 2 2 3 1
cTanmen   = mq6 19 "tanmen"   1 2 3 3 2 1
-- - Three adjacent pairs:
cTamene   = mq6 20 "tamene"   1 3 3 2 2 1
cTaneme   = mq6 21 "taneme"   1 2 2 3 3 1

-- Two 1's rest:
-- - No adjacent pairs:
cNatnarn = mq6 22 "natnarn" 2 1 2 4 2 1
-- - One adjacent pair:
cNatner  = mq6 23 "natner"  2 1 2 2 4 1
cNatrane = mq6 24 "natrane" 2 1 4 2 2 1
cNetnar  = mq6 25 "netnar"  2 2 1 2 4 1
cNetran  = mq6 26 "netran"  2 2 1 4 2 1
cTanern  = mq6 27 "tanern"  1 2 2 4 2 1
cTanrane = mq6 28 "tanrane" 1 2 4 2 2 1
-- - Adjacent triple:
cNitar   = mq6 29 "nitar"   2 2 2 1 4 1
cTanir   = mq6 30 "tanir"   1 2 2 2 4 1
cTrani   = mq6 31 "trani"   1 4 2 2 2 1

-- Three 1's, nothing > 3:
-- No pairs:
cMatmatam = mq6 32 "matmatam" 3 1 3 1 3 1
-- Pairs:
cMateme   = mq6 33 "mateme"   3 1 1 3 3 1
cTamtame  = mq6 34 "tamtame"  1 3 1 3 3 1
-- Triples:
cTemi     = mq6 35 "temi"     1 1 3 3 3 1

-- Three 1's, nothing > 4:
-- - No pairs:
cMatnatar = mq6 36 "matnatar" 3 1 2 1 4 1
cNatmatar = mq6 37 "natmatar" 2 1 3 1 4 1
-- - No triples (implies one pair):
-- . - Highest isolated from other highs:
cManter   = mq6 38 "manter"   3 2 1 1 4 1
cNamter   = mq6 39 "namter"   2 3 1 1 4 1
cTamnatar = mq6 40 "tamnatar" 1 3 2 1 4 1
cTanmatar = mq6 41 "tanmatar" 1 2 3 1 4 1
-- . - 2nd highest isolated from other highs:
cMatenar  = mq6 42 "matenar"  3 1 1 2 4 1
cRantem   = mq6 43 "rantem"   4 2 1 1 3 1
cTamtanar = mq6 44 "tamtanar" 1 3 1 2 4 1
cTrantam  = mq6 45 "trantam"  1 4 2 1 3 1
-- . - Rest:
cNatemar  = mq6 46 "natemar"  2 1 1 3 4 1
cNateram  = mq6 47 "nateram"  2 1 1 4 3 1
cTantmar  = mq6 48 "tantmar"  1 2 1 3 4 1
cTantram  = mq6 49 "tantram"  1 2 1 4 3 1
-- - Triples:
-- . - Outliers separated:
cTemnar   = mq6 50 "temnar"   1 1 3 2 4 1
cTernam   = mq6 51 "ternam"   1 1 4 2 3 1
-- . - High near low:
cTenmar   = mq6 52 "tenmar"   1 1 2 3 4 1
cTerman   = mq6 53 "terman"   1 1 4 3 2 1
-- . - Rest:
cTemran   = mq6 54 "temran"   1 1 3 4 2 1
cTenram   = mq6 55 "tenram"   1 1 2 4 3 1

-- Three 1's rest:
-- - No pairs:
cNatnatl = mq6 56 "natnatl" 2 1 2 1 5 1
-- - Pairs:
-- - - High separated:
cNetel   = mq6 57 "netel"   2 2 1 1 5 1
cTanetal = mq6 58 "tanetal" 1 2 2 1 5 1
-- - - Rest:
cNateln  = mq6 59 "nateln"  2 1 1 5 2 1
cNatenal = mq6 60 "natenal" 2 1 1 2 5 1
cTantlan = mq6 61 "tantlan" 1 2 1 5 2 1
cTantnal = mq6 62 "tantnal" 1 2 1 2 5 1
-- - Triples:
-- - - No pair outside triple:
cTenlan  = mq6 63 "tenlan"  1 1 2 5 2 1
-- - - Rest:
cTelne   = mq6 64 "telne"   1 1 5 2 2 1
cTenel   = mq6 65 "tenel"   1 1 2 2 5 1

-- Four 1's:
-- - No triple:
-- . - Uniform outliers:
cTrater = mq6 66 "trater" 1 4 1 1 4 1
-- . - Medium uniform outliers:
cTamtel = mq6 67 "tamtel" 1 3 1 1 5 1
-- . - Rest:
cTantej = mq6 68 "tantej" 1 2 1 1 6 1
-- - No quadruple:
-- . - Uniform outliers:
cTertar = mq6 69 "tertar" 1 1 4 1 4 1
-- . - Medium uniform outliers:
cMatil  = mq6 70 "matil"  3 1 1 1 5 1
cTemtal = mq6 71 "temtal" 1 1 3 1 5 1
-- . - Rest:
cNatij  = mq6 72 "natij"  2 1 1 1 6 1
cTentaj = mq6 73 "tentaj" 1 1 2 1 6 1
-- - Rest:
-- . - Uniform outliers:
cTire   = mq6 74 "tire"   1 1 1 4 4 1
-- . - Medium uniform outliers:
cTilm   = mq6 75 "tilm"   1 1 1 5 3 1
cTimal  = mq6 76 "timal"  1 1 1 3 5 1
-- . - Rest:
cTijan  = mq6 77 "tijan"  1 1 1 6 2 1
cTinaj  = mq6 78 "tinaj"  1 1 1 2 6 1

-- Rest:
cTok = mq6 79 "tok" 1 1 1 1 7 1
