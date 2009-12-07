<CsoundSynthesizer>
  <CsOptions>
    csound -d -odac -L stdin
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
    f 0 3600
  </CsScore>
</CsoundSynthesizer>
