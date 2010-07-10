module Main where

import AlsaMidi

main = do
  s7r <- midiInitialize "dynmus" "128:0"
  setInstrument s7r 0 1 41
  noteOn s7r 0 0 60 100
  noteOn s7r 200 1 63 50
  noteOff s7r 400 0 60
  noteOff s7r 600 1 63
  midiFinalize s7r
