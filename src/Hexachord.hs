module Hexachord where

import Haskore.Basic.Pitch

import Chord
import Named

mq6 :: Relative -> Relative -> Relative
    -> Relative -> Relative -> Relative
    -> ModeQ
mq6 a b c d e f = mqFromAdjIvls [a, b, c, d, e, f]

cManetam, cMano, cMantanam, cManter, cMantman, cMateme, cMatenar      :: ModeQ
cMatil, cMatmatam, cMatnatar, cMatnem, cNamni, cNamnatam, cNamtanam   :: ModeQ
cNamter, cNatelan, cNatemar, cNatenal, cNateram, cNatij, cNatmanam    :: ModeQ
cNatmatar, cNatmen, cNatname, cNatnaran, cNatnatal, cNatner, cNatrane :: ModeQ
cNemne, cNetel, cNetme, cNetnar, cNetran, cNiman, cNitar, cNom, cNu   :: ModeQ
cRantem, cTamnem, cTamnatar, cTamene, cTamnaman, cTamtame, cTamtanar  :: ModeQ
cTamtel, cTaneme, cTaneran, cTanetal, cTanir, cTanmanam, cTanmatar    :: ModeQ
cTanmen, cTanrane, cTantej, cTantlan, cTantmar, cTantnal, cTantram    :: ModeQ
cTelane, cTemi, cTemnar, cTemran, cTemtal, cTenel, cTenlan, cTenmar   :: ModeQ
cTenram, cTentaj, cTerman, cTernam, cTertar, cTijan, cTilam, cTimal   :: ModeQ
cTinaj, cTire, cTok, cTrani, cTrantam, cTrater                        :: ModeQ

-- 80 hexachords. Number with n minor-2nds:
-- 0:  1
-- 1:  5
-- 2: 26
-- 3: 34
-- 4: 13
-- 5:  1

cNu = mq6 2 2 2 2 2 2

cMano  = mq6 3 2 2 2 2 1
cNamni = mq6 2 3 2 2 2 1
cNemne = mq6 2 2 3 2 2 1
cNiman = mq6 2 2 2 3 2 1
cNom   = mq6 2 2 2 2 3 1

cManetam  = mq6 3 2 2 1 3 1
cMantanam = mq6 3 2 1 2 3 1
cMantman  = mq6 3 2 1 3 2 1
cMatnem   = mq6 3 1 2 2 3 1
cNamnatam = mq6 2 3 2 1 3 1
cNamtanam = mq6 2 3 1 2 3 1
cNatmanam = mq6 2 1 3 2 3 1
cNatmen   = mq6 2 1 3 3 2 1
cNatname  = mq6 2 1 2 3 3 1
cNetme    = mq6 2 2 1 3 3 1
cTamnem   = mq6 1 3 2 2 3 1
cTamene   = mq6 1 3 3 2 2 1
cTamnaman = mq6 1 3 2 3 2 1
cTaneme   = mq6 1 2 2 3 3 1
cTanmanam = mq6 1 2 3 2 3 1
cTanmen   = mq6 1 2 3 3 2 1

cNatnaran = mq6 2 1 2 4 2 1
cNatner   = mq6 2 1 2 2 4 1
cNatrane  = mq6 2 1 4 2 2 1
cNetnar   = mq6 2 2 1 2 4 1
cNetran   = mq6 2 2 1 4 2 1
cNitar    = mq6 2 2 2 1 4 1
cTaneran  = mq6 1 2 2 4 2 1
cTanir    = mq6 1 2 2 2 4 1
cTanrane  = mq6 1 2 4 2 2 1
cTrani    = mq6 1 4 2 2 2 1

cManter   = mq6 3 2 1 1 4 1
cMateme   = mq6 3 1 1 3 3 1
cMatenar  = mq6 3 1 1 2 4 1
cMatmatam = mq6 3 1 3 1 3 1
cMatnatar = mq6 3 1 2 1 4 1
cNamter   = mq6 2 3 1 1 4 1
cNatelan  = mq6 2 1 1 5 2 1
cNatemar  = mq6 2 1 1 3 4 1
cNatenal  = mq6 2 1 1 2 5 1
cNateram  = mq6 2 1 1 4 3 1
cNatmatar = mq6 2 1 3 1 4 1
cNatnatal = mq6 2 1 2 1 5 1
cNetel    = mq6 2 2 1 1 5 1
cRantem   = mq6 4 2 1 1 3 1
cTamnatar = mq6 1 3 2 1 4 1
cTamtame  = mq6 1 3 1 3 3 1
cTamtanar = mq6 1 3 1 2 4 1
cTanetal  = mq6 1 2 2 1 5 1
cTanmatar = mq6 1 2 3 1 4 1
cTantlan  = mq6 1 2 1 5 2 1
cTantmar  = mq6 1 2 1 3 4 1
cTantnal  = mq6 1 2 1 2 5 1
cTantram  = mq6 1 2 1 4 3 1
cTelane   = mq6 1 1 5 2 2 1
cTemi     = mq6 1 1 3 3 3 1
cTemnar   = mq6 1 1 3 2 4 1
cTemran   = mq6 1 1 3 4 2 1
cTenel    = mq6 1 1 2 2 5 1
cTenlan   = mq6 1 1 2 5 2 1
cTenmar   = mq6 1 1 2 3 4 1
cTenram   = mq6 1 1 2 4 3 1
cTerman   = mq6 1 1 4 3 2 1
cTernam   = mq6 1 1 4 2 3 1
cTrantam  = mq6 1 4 2 1 3 1

cMatil  = mq6 3 1 1 1 5 1
cNatij  = mq6 2 1 1 1 6 1
cTamtel = mq6 1 3 1 1 5 1
cTemtal = mq6 1 1 3 1 5 1
cTantej = mq6 1 2 1 1 6 1
cTentaj = mq6 1 1 2 1 6 1
cTertar = mq6 1 1 4 1 4 1
cTijan  = mq6 1 1 1 6 2 1
cTilam  = mq6 1 1 1 5 3 1
cTimal  = mq6 1 1 1 3 5 1
cTinaj  = mq6 1 1 1 2 6 1
cTire   = mq6 1 1 1 4 4 1
cTrater = mq6 1 4 1 1 4 1

cTok = mq6 1 1 1 1 7 1

hexachords :: [Named ModeQ]
hexachords =
    [ Named "manetam"  cManetam  -- 1
    , Named "mano"     cMano 
    , Named "mantanam" cMantanam 
    , Named "manter"   cManter 
    , Named "mantman"  cMantman 
    , Named "mateme"   cMateme   -- 6
    , Named "matenar"  cMatenar 
    , Named "matil"    cMatil 
    , Named "matmatam" cMatmatam 
    , Named "matnatar" cMatnatar 
    , Named "matnem"   cMatnem   -- 11
    , Named "namni"    cNamni 
    , Named "namnatam" cNamnatam 
    , Named "namtanam" cNamtanam 
    , Named "namter"   cNamter 
    , Named "natelan"  cNatelan  -- 16
    , Named "natemar"  cNatemar 
    , Named "natenal"  cNatenal 
    , Named "nateram"  cNateram 
    , Named "natij"    cNatij 
    , Named "natmanam" cNatmanam -- 21
    , Named "natmatar" cNatmatar 
    , Named "natmen"   cNatmen 
    , Named "natname"  cNatname 
    , Named "natnaran" cNatnaran 
    , Named "natnatal" cNatnatal -- 26
    , Named "natner"   cNatner 
    , Named "natrane"  cNatrane 
    , Named "netel"    cNetel 
    , Named "netme"    cNetme    -- 31
    , Named "netnar"   cNetnar 
    , Named "netran"   cNetran 
    , Named "niman"    cNiman 
    , Named "nitar"    cNitar 
    , Named "nom"      cNom      -- 36
    , Named "nu"       cNu 
    , Named "rantem"   cRantem 
    , Named "tamnem"   cTamnem 
    , Named "tamnatar" cTamnatar 
    , Named "tamene"   cTamene   -- 41
    , Named "tamnaman" cTamnaman 
    , Named "tamtame"  cTamtame 
    , Named "tamtanar" cTamtanar 
    , Named "tamtel"   cTamtel 
    , Named "taneme"   cTaneme   -- 46
    , Named "taneran"  cTaneran 
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
    , Named "telane"   cTelane 
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
    , Named "tilam"    cTilam 
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
