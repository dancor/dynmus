{-# LANGUAGE BangPatterns #-}

module Note where

import Dynmus

data Note a = Note
    { nNotesPerOctave :: !Int
    , nOctave         :: !Int
    , nNote           :: !a
    }

type NoteNum = Int

readNoteLtr :: String -> Int
readNoteLtr "C"  = nC
readNoteLtr "Db" = nDb
readNoteLtr "D"  = nD
readNoteLtr "Eb" = nEb
readNoteLtr "E"  = nE
readNoteLtr "F"  = nF
readNoteLtr "Gb" = nGb
readNoteLtr "G"  = nG
readNoteLtr "Ab" = nAb
readNoteLtr "A"  = nA
readNoteLtr "Bb" = nBb
readNoteLtr "B"  = nB

nC, nDb, nD, nEb, nE, nF, nGb, nG, nAb, nA, nBb, nB :: Int
nC  = 0
nDb = 1
nD  = 2
nEb = 3
nE  = 4
nF  = 5
nGb = 6
nG  = 7
nAb = 8
nA  = 9
nBb = 10
nB  = 11

-- | noteNum nC0 = 0
noteNum :: Note Int -> NoteNum
noteNum (Note p o n) = p * o + n

noteNumFreq :: NoteNum -> Freq
noteNumFreq n = 440 * 2 ** ((fromIntegral n - 57) / 12)

class NoteFreq a where
    noteFreq :: Note a -> Freq

instance NoteFreq Int where
    noteFreq !(Note notesPerOctave octave note) =
        440 * 2 ** theExp
      where
        theExp :: Float
        theExp = fromIntegral ((octave - 4) * notesPerOctave + note - 4) /
            fromIntegral notesPerOctave

instance NoteFreq Float where
    noteFreq !(Note notesPerOctave octave note) =
        440 * 2 ** theExp
      where
        theExp :: Float
        theExp = (fromIntegral ((octave - 4) * notesPerOctave) + note - 4) /
            fromIntegral notesPerOctave

nC0, nDb0, nD0, nEb0, nE0, nF0, nGb0, nG0, nAb0, nA0, nBb0, nB0 :: Note Int
nC0  = Note 12 0 0
nDb0 = Note 12 0 1
nD0  = Note 12 0 2
nEb0 = Note 12 0 3
nE0  = Note 12 0 4
nF0  = Note 12 0 5
nGb0 = Note 12 0 6
nG0  = Note 12 0 7
nAb0 = Note 12 0 8
nA0  = Note 12 0 9
nBb0 = Note 12 0 10
nB0  = Note 12 0 11

nC1, nDb1, nD1, nEb1, nE1, nF1, nGb1, nG1, nAb1, nA1, nBb1, nB1 :: Note Int
nC1  = Note 12 1 0
nDb1 = Note 12 1 1
nD1  = Note 12 1 2
nEb1 = Note 12 1 3
nE1  = Note 12 1 4
nF1  = Note 12 1 5
nGb1 = Note 12 1 6
nG1  = Note 12 1 7
nAb1 = Note 12 1 8
nA1  = Note 12 1 9
nBb1 = Note 12 1 10
nB1  = Note 12 1 11

nC2, nDb2, nD2, nEb2, nE2, nF2, nGb2, nG2, nAb2, nA2, nBb2, nB2 :: Note Int
nC2  = Note 12 2 0
nDb2 = Note 12 2 1
nD2  = Note 12 2 2
nEb2 = Note 12 2 3
nE2  = Note 12 2 4
nF2  = Note 12 2 5
nGb2 = Note 12 2 6
nG2  = Note 12 2 7
nAb2 = Note 12 2 8
nA2  = Note 12 2 9
nBb2 = Note 12 2 10
nB2  = Note 12 2 11

nC3, nDb3, nD3, nEb3, nE3, nF3, nGb3, nG3, nAb3, nA3, nBb3, nB3 :: Note Int
nC3  = Note 12 3 0
nDb3 = Note 12 3 1
nD3  = Note 12 3 2
nEb3 = Note 12 3 3
nE3  = Note 12 3 4
nF3  = Note 12 3 5
nGb3 = Note 12 3 6
nG3  = Note 12 3 7
nAb3 = Note 12 3 8
nA3  = Note 12 3 9
nBb3 = Note 12 3 10
nB3  = Note 12 3 11

nC4, nDb4, nD4, nEb4, nE4, nF4, nGb4, nG4, nAb4, nA4, nBb4, nB4 :: Note Int
nC4  = Note 12 4 0
nDb4 = Note 12 4 1
nD4  = Note 12 4 2
nEb4 = Note 12 4 3
nE4  = Note 12 4 4
nF4  = Note 12 4 5
nGb4 = Note 12 4 6
nG4  = Note 12 4 7
nAb4 = Note 12 4 8
nA4  = Note 12 4 9
nBb4 = Note 12 4 10
nB4  = Note 12 4 11

nC5, nDb5, nD5, nEb5, nE5, nF5, nGb5, nG5, nAb5, nA5, nBb5, nB5 :: Note Int
nC5  = Note 12 5 0
nDb5 = Note 12 5 1
nD5  = Note 12 5 2
nEb5 = Note 12 5 3
nE5  = Note 12 5 4
nF5  = Note 12 5 5
nGb5 = Note 12 5 6
nG5  = Note 12 5 7
nAb5 = Note 12 5 8
nA5  = Note 12 5 9
nBb5 = Note 12 5 10
nB5  = Note 12 5 11

nC6, nDb6, nD6, nEb6, nE6, nF6, nGb6, nG6, nAb6, nA6, nBb6, nB6 :: Note Int
nC6  = Note 12 6 0
nDb6 = Note 12 6 1
nD6  = Note 12 6 2
nEb6 = Note 12 6 3
nE6  = Note 12 6 4
nF6  = Note 12 6 5
nGb6 = Note 12 6 6
nG6  = Note 12 6 7
nAb6 = Note 12 6 8
nA6  = Note 12 6 9
nBb6 = Note 12 6 10
nB6  = Note 12 6 11

nC7, nDb7, nD7, nEb7, nE7, nF7, nGb7, nG7, nAb7, nA7, nBb7, nB7 :: Note Int
nC7  = Note 12 7 0
nDb7 = Note 12 7 1
nD7  = Note 12 7 2
nEb7 = Note 12 7 3
nE7  = Note 12 7 4
nF7  = Note 12 7 5
nGb7 = Note 12 7 6
nG7  = Note 12 7 7
nAb7 = Note 12 7 8
nA7  = Note 12 7 9
nBb7 = Note 12 7 10
nB7  = Note 12 7 11

nC8, nDb8, nD8, nEb8, nE8, nF8, nGb8, nG8, nAb8, nA8, nBb8, nB8 :: Note Int
nC8  = Note 12 8 0
nDb8 = Note 12 8 1
nD8  = Note 12 8 2
nEb8 = Note 12 8 3
nE8  = Note 12 8 4
nF8  = Note 12 8 5
nGb8 = Note 12 8 6
nG8  = Note 12 8 7
nAb8 = Note 12 8 8
nA8  = Note 12 8 9
nBb8 = Note 12 8 10
nB8  = Note 12 8 11

nC9, nDb9, nD9, nEb9, nE9, nF9, nGb9, nG9, nAb9, nA9, nBb9, nB9 :: Note Int
nC9  = Note 12 9 0
nDb9 = Note 12 9 1
nD9  = Note 12 9 2
nEb9 = Note 12 9 3
nE9  = Note 12 9 4
nF9  = Note 12 9 5
nGb9 = Note 12 9 6
nG9  = Note 12 9 7
nAb9 = Note 12 9 8
nA9  = Note 12 9 9
nBb9 = Note 12 9 10
nB9  = Note 12 9 11

nC10, nDb10, nD10, nEb10, nE10, nF10, nGb10, nG10, nAb10, nA10, nBb10,
    nB10 :: Note Int
nC10  = Note 12 10 0
nDb10 = Note 12 10 1
nD10  = Note 12 10 2
nEb10 = Note 12 10 3
nE10  = Note 12 10 4
nF10  = Note 12 10 5
nGb10 = Note 12 10 6
nG10  = Note 12 10 7
nAb10 = Note 12 10 8
nA10  = Note 12 10 9
nBb10 = Note 12 10 10
nB10  = Note 12 10 11

nC11, nDb11, nD11, nEb11, nE11, nF11, nGb11, nG11, nAb11, nA11, nBb11,
    nB11 :: Note Int
nC11  = Note 12 11 0
nDb11 = Note 12 11 1
nD11  = Note 12 11 2
nEb11 = Note 12 11 3
nE11  = Note 12 11 4
nF11  = Note 12 11 5
nGb11 = Note 12 11 6
nG11  = Note 12 11 7
nAb11 = Note 12 11 8
nA11  = Note 12 11 9
nBb11 = Note 12 11 10
nB11  = Note 12 11 11
