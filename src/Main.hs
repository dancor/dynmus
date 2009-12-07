{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad.Error
import Heredoc (here)
import Sound.Csound

csd = [$here|<CsoundSynthesizer>
  <CsOptions>
    csound -d
  </CsOptions>
  <CsInstruments>
    sr     = 44100           ; Sample rate.
    kr     = 4410            ; Control signal rate.
    ksmps  = 10              ; Samples pr. control signal.
    nchnls = 1               ; Number of output channels.
    instr 1
    a1     oscil p4, p5, 1   ; Simple oscillator.
           out a1            ; Output.
    endin
  </CsInstruments>
  <CsScore>
    f1 0 8192 10 1           ; Table containing a sine wave.
    i1 0 1 20000 1000        ; Play one second of one kHz tone.
    e
  </CsScore>
</CsoundSynthesizer>|]

runCsoundSimple :: String -> CsoundMonad CsoundPerformStatus
runCsoundSimple = undefined

main :: IO ()
main = do
  print "hi"
  res <- runErrorT $ runCsoundSimple csd
  print res

