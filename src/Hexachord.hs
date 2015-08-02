module Hexachord where

import Haskore.Basic.Pitch

import Chord
import Named

mq6 :: Relative -> Relative -> Relative
    -> Relative -> Relative -> Relative
    -> ModeQ
mq6 a b c d e f = mqFromAdjIvls [a, b, c, d, e, f]

cManetam, cMano, cMantnam, cManter, cMantman, cMateme, cMatenar      :: ModeQ
cMatil, cMatmatam, cMatnatar, cMatnem, cNamni, cNamnatam, cNamtanam  :: ModeQ
cNamter, cNateln, cNatemar, cNatenal, cNateram, cNatij, cNatmanam    :: ModeQ
cNatmatar, cNatmen, cNatname, cNatnarn, cNatnatl, cNatner, cNatrane  :: ModeQ
cNemne, cNetel, cNetme, cNetnar, cNetran, cNiman, cNitar, cNom, cNu  :: ModeQ
cRantem, cTamnem, cTamnatar, cTamene, cTamnaman, cTamtame, cTamtanar :: ModeQ
cTamtel, cTaneme, cTanern, cTanetal, cTanir, cTanmanam, cTanmatar    :: ModeQ
cTanmen, cTanrane, cTantej, cTantlan, cTantmar, cTantnal, cTantram   :: ModeQ
cTelne, cTemi, cTemnar, cTemran, cTemtal, cTenel, cTenlan, cTenmar   :: ModeQ
cTenram, cTentaj, cTerman, cTernam, cTertar, cTijan, cTilm, cTimal   :: ModeQ
cTinaj, cTire, cTok, cTrani, cTrantam, cTrater                       :: ModeQ

-- 80 hexachords. Number with n minor-2nds:
-- 0:  1
-- 1:  5
-- 2: 26
-- 3: 34
-- 4: 13
-- 5:  1

-- - Zero 1's:
cNu = mq6 2 2 2 2 2 2

-- - One 1:
-- - No adjacent triples:
cNemne = mq6 2 2 3 2 2 1
-- - No adjacent quadruples:
cNamni = mq6 2 3 2 2 2 1
cNiman = mq6 2 2 2 3 2 1
-- - Adjacent quadruples:
cMano  = mq6 3 2 2 2 2 1
cNom   = mq6 2 2 2 2 3 1

-- Two 1's, nothing > 3:
-- - No single-separated or adjacent pairs:
cMantman  = mq6 3 2 1 3 2 1
cNamtanam = mq6 2 3 1 2 3 1
-- - No adjacent pairs:
cMantnam  = mq6 3 2 1 2 3 1
cNamnatam = mq6 2 3 2 1 3 1
cNatmanam = mq6 2 1 3 2 3 1
-- - One adjacent pair:
cManetam  = mq6 3 2 2 1 3 1
cMatnem   = mq6 3 1 2 2 3 1
cNatmen   = mq6 2 1 3 3 2 1
cNatname  = mq6 2 1 2 3 3 1
cTamnaman = mq6 1 3 2 3 2 1
cTanmanam = mq6 1 2 3 2 3 1
-- - Two adjacent pairs:
cNetme    = mq6 2 2 1 3 3 1
cTamnem   = mq6 1 3 2 2 3 1
cTanmen   = mq6 1 2 3 3 2 1
-- - Three adjacent pairs:
cTamene   = mq6 1 3 3 2 2 1
cTaneme   = mq6 1 2 2 3 3 1

-- Two 1's rest:
-- - No adjacent pairs:
cNatnarn = mq6 2 1 2 4 2 1
-- - One adjacent pair:
cNatner  = mq6 2 1 2 2 4 1
cNatrane = mq6 2 1 4 2 2 1
cNetnar  = mq6 2 2 1 2 4 1
cNetran  = mq6 2 2 1 4 2 1
cTanern  = mq6 1 2 2 4 2 1
cTanrane = mq6 1 2 4 2 2 1
-- - Adjacent triple:
cNitar   = mq6 2 2 2 1 4 1
cTanir   = mq6 1 2 2 2 4 1
cTrani   = mq6 1 4 2 2 2 1

-- Three 1's, nothing > 3:
-- No pairs:
cMatmatam = mq6 3 1 3 1 3 1
-- Pairs:
cMateme   = mq6 3 1 1 3 3 1
cTamtame  = mq6 1 3 1 3 3 1
-- Triples:
cTemi     = mq6 1 1 3 3 3 1

-- Three 1's, nothing > 4:
-- - No pairs:
cMatnatar = mq6 3 1 2 1 4 1
cNatmatar = mq6 2 1 3 1 4 1
-- - No triples (implies one pair):
-- . - Highest isolated from other highs:
cManter   = mq6 3 2 1 1 4 1
cNamter   = mq6 2 3 1 1 4 1
cTamnatar = mq6 1 3 2 1 4 1
cTanmatar = mq6 1 2 3 1 4 1
-- . - 2nd highest isolated from other highs:
cMatenar  = mq6 3 1 1 2 4 1
cRantem   = mq6 4 2 1 1 3 1
cTamtanar = mq6 1 3 1 2 4 1
cTrantam  = mq6 1 4 2 1 3 1
-- . - Rest:
cNatemar  = mq6 2 1 1 3 4 1
cNateram  = mq6 2 1 1 4 3 1
cTantmar  = mq6 1 2 1 3 4 1
cTantram  = mq6 1 2 1 4 3 1
-- - Triples:
-- . - Outliers separated:
cTemnar   = mq6 1 1 3 2 4 1
cTernam   = mq6 1 1 4 2 3 1
-- . - High near low:
cTenmar   = mq6 1 1 2 3 4 1
cTerman   = mq6 1 1 4 3 2 1
-- . - Rest:
cTemran   = mq6 1 1 3 4 2 1
cTenram   = mq6 1 1 2 4 3 1

-- Three 1's rest:
-- - No pairs:
cNatnatl = mq6 2 1 2 1 5 1
-- - Pairs:
-- - - High separated:
cNetel   = mq6 2 2 1 1 5 1
cTanetal = mq6 1 2 2 1 5 1
-- - - Rest:
cNateln  = mq6 2 1 1 5 2 1
cNatenal = mq6 2 1 1 2 5 1
cTantlan = mq6 1 2 1 5 2 1
cTantnal = mq6 1 2 1 2 5 1
-- - Triples:
-- - - No pair outside triple:
cTenlan  = mq6 1 1 2 5 2 1
-- - - Rest:
cTelne   = mq6 1 1 5 2 2 1
cTenel   = mq6 1 1 2 2 5 1

-- Four 1's:
-- - No triple:
-- . - Uniform outliers:
cTrater = mq6 1 4 1 1 4 1
-- . - Medium uniform outliers:
cTamtel = mq6 1 3 1 1 5 1
-- . - Rest:
cTantej = mq6 1 2 1 1 6 1
-- - No quadruple:
-- . - Uniform outliers:
cTertar = mq6 1 1 4 1 4 1
-- . - Medium uniform outliers:
cMatil  = mq6 3 1 1 1 5 1
cTemtal = mq6 1 1 3 1 5 1
-- . - Rest:
cNatij  = mq6 2 1 1 1 6 1
cTentaj = mq6 1 1 2 1 6 1
-- - Rest:
-- . - Uniform outliers:
cTire   = mq6 1 1 1 4 4 1
-- . - Medium uniform outliers:
cTilm   = mq6 1 1 1 5 3 1
cTimal  = mq6 1 1 1 3 5 1
-- . - Rest:
cTijan  = mq6 1 1 1 6 2 1
cTinaj  = mq6 1 1 1 2 6 1

-- Rest:
cTok = mq6 1 1 1 1 7 1

hexachords :: [Named ModeQ]
hexachords =
    [ Named "manetam"  cManetam  -- 1
    , Named "mano"     cMano
    , Named "manter"   cManter
    , Named "mantman"  cMantman
    , Named "mantnam"  cMantnam
    , Named "mateme"   cMateme   -- 6
    , Named "matenar"  cMatenar
    , Named "matil"    cMatil
    , Named "matmatam" cMatmatam
    , Named "matnatar" cMatnatar
    , Named "matnem"   cMatnem   -- 11
    , Named "namnatam" cNamnatam
    , Named "namni"    cNamni
    , Named "namtanam" cNamtanam
    , Named "namter"   cNamter
    , Named "nateln"   cNateln   -- 16
    , Named "natemar"  cNatemar
    , Named "natenal"  cNatenal
    , Named "nateram"  cNateram
    , Named "natij"    cNatij
    , Named "natmanam" cNatmanam -- 21
    , Named "natmatar" cNatmatar
    , Named "natmen"   cNatmen
    , Named "natname"  cNatname
    , Named "natnarn"  cNatnarn
    , Named "natnatl"  cNatnatl -- 26
    , Named "natner"   cNatner
    , Named "natrane"  cNatrane
    , Named "nemne"    cNemne
    , Named "netel"    cNetel
    , Named "netme"    cNetme    -- 31
    , Named "netnar"   cNetnar
    , Named "netran"   cNetran
    , Named "niman"    cNiman
    , Named "nitar"    cNitar
    , Named "nom"      cNom      -- 36
    , Named "nu"       cNu
    , Named "rantem"   cRantem
    , Named "tamene"   cTamene
    , Named "tamnaman" cTamnaman
    , Named "tamnatar" cTamnatar -- 41
    , Named "tamnem"   cTamnem
    , Named "tamtame"  cTamtame
    , Named "tamtanar" cTamtanar
    , Named "tamtel"   cTamtel
    , Named "taneme"   cTaneme   -- 46
    , Named "tanern"   cTanern
    , Named "tanetal"  cTanetal
    , Named "tanir"    cTanir
    , Named "tanmanam" cTanmanam
    , Named "tanmatar" cTanmatar -- 51
    , Named "tanmen"   cTanmen
    , Named "tanrane"  cTanrane
    , Named "tantej"   cTantej
    , Named "tantlan"  cTantlan
    , Named "tantmar"  cTantmar  -- 56
    , Named "tantnal"  cTantnal
    , Named "tantram"  cTantram
    , Named "telne"    cTelne
    , Named "temi"     cTemi
    , Named "temnar"   cTemnar   -- 61
    , Named "temran"   cTemran
    , Named "temtal"   cTemtal
    , Named "tenel"    cTenel
    , Named "tenlan"   cTenlan
    , Named "tenmar"   cTenmar   -- 66
    , Named "tenram"   cTenram
    , Named "tentaj"   cTentaj
    , Named "terman"   cTerman
    , Named "ternam"   cTernam
    , Named "tertar"   cTertar   -- 71
    , Named "tijan"    cTijan
    , Named "tilm"     cTilm
    , Named "timal"    cTimal
    , Named "tinaj"    cTinaj
    , Named "tire"     cTire     -- 76
    , Named "tok"      cTok
    , Named "trani"    cTrani
    , Named "trantam"  cTrantam
    , Named "trater"   cTrater   -- 80
    ]

goodHexachords :: [Named ModeQ]
goodHexachords =
    [ Named "mano"     cMano
    , Named "namni"    cNamni
    , Named "nemne"    cNemne
    , Named "niman"    cNiman
    , Named "nom"      cNom
    ]
