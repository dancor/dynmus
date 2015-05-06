module LolHaskore
    ( module LolHaskore
    , module Haskore.Basic.Duration
    , module Haskore.Music
    , Absolute
    , Relative
    , note
    , na
    ) where

import Haskore.Basic.Duration hiding (T)
import Haskore.Basic.Pitch as Pitch
import Haskore.Melody as Mel
import Haskore.Melody.Standard
import Haskore.Music hiding (filter, replicate, take)
import Haskore.Music.GeneralMIDI as MidiMus
import Haskore.Interface.MIDI.Render
import System.Process

type Mus = Mel.T NoteAttributes

playPiano :: Mus -> IO ()
playPiano m = do
    fileFromGeneralMIDIMusic "tmp" $
        MidiMus.fromStdMelody MidiMus.AcousticGrandPiano m
    _ <- readProcess "playmidi" ["tmp"] ""
    return ()

{-
chords :: Mus
chords =
    (c 0 qn na =:= e 0 qn na =:= g 0 qn na) +:+
    (c 0 qn na =:= f 0 qn na =:= a 0 qn na) +:+
    (d 0 qn na =:= g 0 qn na =:= b 0 qn na)

main :: IO ()
main = do
    -- Haskore's bare tempo is only 15 quarter note bpm!
    playPiano $ transpose (-3) $ changeTempo 1 $ chords
    {-
    line
        [ c 0 qn na
        , d 0 qn na
        , e 0 qn na
        , f 0 qn na
        , g 0 qn na
        , a 0 qn na
        , b 0 qn na
        , c 1 qn na
        ]
        -}
        -}
