module Note where

import Freq

data Note = Note !Int !Int !Int

noteFreq :: Note -> Freq
noteFreq !(Note notesPerOctave octave note) =
    440 * 2 ** theExp
  where
    theExp :: Float
    theExp = fromIntegral ((octave - 4) * notesPerOctave + note) /
        fromIntegral notesPerOctave
