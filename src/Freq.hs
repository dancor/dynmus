module Freq where

type Freq = Float

sampleRate :: Int
sampleRate = 44100

sampRateF :: Float
sampRateF = fromIntegral sampleRate
