{-# LANGUAGE BangPatterns #-}

module Note where

import Freq

data Note a = Note
    { nNotesPerOctave :: !Int
    , nOctave         :: !Int
    , nNote           :: !a
    }

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
