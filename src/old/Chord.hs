module Chord where

import Note
import ChordBase

-- First is bass note, rest are unordered.
data Chord = [Int]

type BeatDur = Rational

myHarmonyMelody :: [((Chord, Note), BeatDur)]
myHarmonyMelody = relative c'
    [ bMaj, r4, b, dis, fis
    , gMaj, g1
    , bMaj, fis
    ,       r4, b_, dis, fis
    , gMaj, g, fis, eMin, b4d, cis8
    , bMaj, b2d, b4
    ,       b4d, ais8, disMaj, ais4d, gis8
    , eMaj, gis2d, gis4
    ,       gis4d, ais8, gis4d, fis8
    , bMaj, fis2, r4, b_
    , cisMin7 `over` g, e4d, dis8, cis4d, dis8
    , fMaj4, cis1
    , fMaj, cis
    ]

relative :: ChordOrLaod -> [ChordOrLaod] -> [((Chord, Note), NoteDur)]
relative =

type ChordOrLaod = Either Chord (Maybe (Int, Int, Int), BeatDur)

ltr :: Int -> ChordOrLaod -> ChordOrLaod
ltr x (Right (Just (_, a, b), c)) = Right (Just (x, a, b), c)
ltr _ = error "Error calling ltr."

acc :: Int -> ChordOrLaod -> ChordOrLaod
acc x (Right (Just (a, _, b), c)) = Right (Just (a, x, b), c)
acc _ = error "Error calling acc."

oct :: Int -> ChordOrLaod -> ChordOrLaod
oct x (Right (Just (a, b, _), c)) = Right (Just (a, b, x), c)
oct _ = error "Error calling oct."

dur :: BeatDur -> ChordOrLaod -> ChordOrLaod
dur x (Right (a, _)) = Right (a, x)
dur _ = error "Error calling dur."

dotted :: BeatDur -> BeatDur
dotted = (3/2 *)

r, r1, r2, r2d, r4, r4d :: ChordOrNoteOctDur
r = Right (Nothing, 0)
r1 = Right (Nothing, 1)
r2 = Right (Nothing, 1/2)
r2d = Right (Nothing, dotted (1/2))
r4 = Right (Nothing, 1/4)
r4d = Right (Nothing, dotted (1/4))
r8 = Right (Nothing, 1/8)
r8d = Right (Nothing, dotted (1/8))
r16 = Right (Nothing, 1/16)
r16d = Right (Nothing, dotted (1/16))

c, c', c_ :: ChordOrNoteOctDur
c = Right (Just (0, 0), 0, 0)
c' = Right (Just (0, 0), 1, 0)
c'' = Right (Just (0, 0), 2, 0)
c_ = Right (Just (0, 0), -1, 0)
c__ = Right (Just (0, 0), -2, 0)

i2, im3, i3, i4, iT, i5, im6, i6, im7, i7 :: Int
i2  = 2
im3 = 3
i3  = 4
i4  = 5
iT  = 6
i5  = 7
im6 = 8
i6  = 9
im7 = 10
i7  = 11

majOffs, maj7Offs, maj4Offs :: Chord
majOffs = [i3, i5]
maj4Offs = majOffs ++ [i4]
maj7Offs = majOffs ++ [im7]
maj47Offs = maj4Offs ++ [im7]

minOffs, min7Offs, maj7Offs :: Chord
minOffs = [im3, i5]
min4Offs = minOffs ++ [i4]
min7Offs = minOffs ++ [im7]
min47Offs = min4Offs ++ [im7]

dimOffs, dim7Offs, dim7Offs :: Chord
dimOffs = [im3, iT]
dim4Offs = dimOffs ++ [i4]
dim7Offs = dimOffs ++ [i6]
dim47Offs = dim4Offs ++ [i6]
hdim7Offs = dimOffs ++ [im7]
hdim47Offs = dim4Offs ++ [im7]

augOffs, aug7Offs, aug7Offs :: Chord
augOffs = [i3, im6]
aug4Offs = augOffs ++ [i4]
aug7Offs = augOffs ++ [im7]
aug47Offs = aug4Offs ++ [im7]

offsOn :: Int -> Chord -> Chord
offsOn n offs = map (`rem` 12) $ map (+ n) offs

cMaj, cMaj4, cMaj7, cMaj47 :: Chord
cMaj = offsOn 0 majOffs
cMaj4 = offsOn 0 maj4Offs
cMaj7 = offsOn 0 maj7Offs
cMaj47 = offsOn 0 maj47Offs

cMin, cMin4, cMin7, cMin47 :: Chord
cMin = offsOn 0 minOffs
cMin4 = offsOn 0 min4Offs
cMin7 = offsOn 0 min7Offs
cMin47 = offsOn 0 min47Offs

cDim, cDim4, cDim7, cDim47 :: Chord
cDim = offsOn 0 minOffs
cDim4 = offsOn 0 min4Offs
cDim7 = offsOn 0 min7Offs
cDim47 = offsOn 0 min47Offs
cHdim7 = offsOn 0 hdim7Offs
cHdim47 = offsOn 0 hdim7Offs

cAug, cAug4, cAug7, cAug47 :: Chord
cAug = offsOn 0 augOffs
cAug4 = offsOn 0 aug4Offs
cAug7 = offsOn 0 aug7Offs
cAug47 = offsOn 0 aug7Offs
