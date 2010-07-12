module Main where

import Control.Applicative
import Data.List

import AlsaMidi

type Voice = [VoicePart]

data VoicePart = VoiceNote Note Vel Tick | VoicePause Tick
  deriving (Eq, Ord, Show)

data MidiEvent = 
  SetInstr Instr | 
  NoteOn Note Vel |
  NoteOff Note
  deriving (Eq, Ord, Show)

type Midi = [((Tick, Chan), MidiEvent)]

playMidi :: S7r -> Midi -> IO ()
playMidi s7r = mapM_ $ \ a@((t, c), e) -> print a >> case e of
  SetInstr i -> setInstrument s7r t c i
  NoteOn n v -> noteOn s7r t c n v
  NoteOff n -> noteOff s7r t c n

voiceToMidi :: Chan -> Voice -> Midi
voiceToMidi c v0 = snd $ foldl' (\ (tCur, a) e -> case e of
  VoiceNote n v t -> 
    (tNew, a ++ [((tCur, c), NoteOn n v), ((tNew, c), NoteOff n)])
    where tNew = tickSum tCur t
  VoicePause t -> (tickSum tCur t, a)
  ) (Tick 0, []) v0

myVoice = 
  VoiceNote (Note 60) (Vel 70) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 70) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 70) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 70) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 60) (Vel 50) (Tick 48) :
  VoiceNote (Note 48) (Vel 80) (Tick $ 48 * 4 * 2) :

  {-
  VoiceNote (Note 60) (Vel 50) (Tick $ 3 * 48) :
  VoiceNote (Note 62) (Vel 50) (Tick 96) :
  VoiceNote (Note 63) (Vel 50) (Tick 96) :
  VoiceNote (Note 68) (Vel 50) (Tick 336) :
  VoiceNote (Note 67) (Vel 100) (Tick 48) :
  VoicePause (Tick 144) :
  VoiceNote (Note 68) (Vel 50) (Tick 144) :
  VoiceNote (Note 67) (Vel 100) (Tick 48) :
  -}
  []

-- takes 2 ticks to reliably change instrument..
-- takes more to get even start not sure how many but 48 seems to work ok
-- can't change as we go (hangs at end)?  just permanent instruments for now.
myMidi = 
  ((Tick 0, Chan 0), SetInstr (Instr 41)) : 
  ((Tick 0, Chan 1), SetInstr (Instr 43)) : 
  ((Tick 0, Chan 1), SetInstr (Instr 43)) : 
  ((Tick 0, Chan 1), SetInstr (Instr 43)) : 
  --voiceToMidi (Chan 1) (VoicePause (Tick 2) : myVoice)
  voiceToMidi (Chan 1) (VoicePause (Tick 48) : myVoice)
  {-
  ((Tick 2, Chan 1), NoteOn (Note 60) (Vel 100)) : 
  ((Tick 300, Chan 1), NoteOff (Note 60)) : 
  ((Tick 410, Chan 1), NoteOn (Note 60) (Vel 100)) : 
  ((Tick 600, Chan 1), NoteOff (Note 60)) : 
  []
  -}

main = do
  s7r <- midiInitialize "dynmus" "128:0"
  --setInstrument s7r (Tick 0) (Chan 0) (Instr 41)
  --noteOn s7r (Tick 100) (Chan 0) (Note 60) (Vel 100)
  playMidi s7r myMidi
  --noteOff s7r (Tick 300) (Chan 0) (Note 60)
  {-
  tick <- steadyLine s7r 0 20 1 100 [60, 64, 68, 64, 60]
  tick2 <- steadyLine s7r tick 20 1 100 $ 
    map (subtract 2) [60, 64, 68, 64, 60]
  tick3 <- steadyLine s7r tick2 20 1 100 $ 
    map (subtract 4) [60, 64, 68, 64, 60]
  -}
  midiFinalize s7r

