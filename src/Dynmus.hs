{-# LANGUAGE BangPatterns #-}

module Dynmus where

import Data.Fixed
import Data.List
import qualified Data.Vector.Unboxed as DVU

import Freq
import Portaudio
import SampTbl

type Val = Ratio Int

type Time = Val

type Duration = Val

type Portion = Val

type Loudness = Val

type Sampled a = [a]

type Samples = Sampled Val

sampleRate :: Int
sampleRate = 44100

ticks :: Time -> Int
ticks t = floor (t / fromIntegral sampleRate)

ratParts :: Val -> (Int, Val)
ratParts x = (q, r % d)
  where
    n = numerator x
    d = denomenator x
    (q, r) = n `quotRem` d

-- | Sample a piecewise constant signal. E.g.: -__--_
sampleConst :: [(Val, Duration)] -> (Samples, Duration)
sampleConst = loop 0
  where
    loop _ [] = []
    loop !subTick ((v, d):vds) = replicate ticks v ++ loop subTick' vds
      where
        endT = subTick + duration
        (ticks, subTick') = ratParts endT
    
-- | Sample a piecewise linear signal. E.g.: /\__/
sampleLinear :: [(Val, Duration)] -> Val -> Samples
sampleLinear = loop 0
  where
    loop _ [] _ = []
    loop !subTick [(v, d)] endVal =
        replicate ticks v ++ loop subTick' vds
    loop !subTick ((v, d):(v2, d2):vds) endVal =
        replicate ticks v ++ loop subTick' ((v2, d2):vds) endVal
      where
        endT = subTick + duration
        (ticks, subTick') = ratParts endT

realizeFreqs :: SampTbl -> [Freq] -> [Sample]
realizeFreqs !tbl =
    snd . mapAccumL doAdvance 0 . map (* tblRateFactor)
  where
    tblSizeF = fromIntegral $ DVU.length tbl
    tblRateFactor = tblSizeF / sampleRateF

    doAdvance :: Float -> Float -> (Float, Sample)
    doAdvance tblPos tblAdvance = (newTblPos, tbl DVU.! floor tblPos)
      where
        newTblPos = (tblPos + tblAdvance) `mod'` tblSizeF

makeEnvel :: Time -> Portion -> [Loudness]
makeEnvel attackHoldDur legatoNess = attack ++ hold ++ fade
  where
    attackDur = 0.25
    fadeDur = 0.5
    holdDur = attackHoldDur - attackDur
    
    
    attack = ticks aDur
    hold = ticks hDur
    [fromIntegral x / fromIntegral onsetTime | x <- [1 .. onsetTime]] ++
    replicate midTime 1 ++
    [fromIntegral x / fromIntegral fallTime | x <- reverse [1 .. fallTime]]
  where
    totalTime = sampleRate * 2
    onsetTime = sampleRate `div` 5
    midTime = totalTime - onsetTime - fallTime
    fallTime = sampleRate `div` 8

