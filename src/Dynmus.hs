{-# LANGUAGE BangPatterns #-}

module Dynmus where

import Data.Fixed
import Data.List
import qualified Data.Vector.Unboxed as DVU

import SampTbl

type Val = Float

type Freq = Val

type Time = Val

type Duration = Val

type Portion = Val

type Loudness = Val

type Sampled a = [a]

type Signal = Sampled Val

-- This could lead to inaccuracies since the sampleRate is not cleanly
-- divisible by say 11. Using 11-tuples could still be okay with some kind
-- of resetting.  The inaccuracies would probably never accumulate to a
-- noticeable point anyway?
ticks :: Time -> Int
ticks t = floor (t * fromIntegral sampleRate)

-- | Sample a piecewise constant signal. E.g.: -__--_
sampleConst :: [(Val, Duration)] -> Signal
sampleConst = concatMap (\(v, d) -> replicate (ticks d) v)

-- | Sample a piecewise linear signal. E.g.: /\__/
sampleLinear :: [(Val, Duration)] -> Val -> Signal
sampleLinear [] _ = []
sampleLinear [(v, d)] vEnd = interp v vEnd d
sampleLinear ((v1, d1):(v2, d2):vds) vEnd =
    interp v1 v2 d1 ++ sampleLinear ((v2, d2):vds) vEnd

interp :: Val -> Val -> Duration -> Signal
interp v1 v2 d = take (floor ticksF) [v1, v1 + (v2 - v1) / ticksF ..]
  where
    ticksF = d * sampleRateF

realizeFreqs :: SampTbl -> [Freq] -> Signal
realizeFreqs !tbl =
    snd . mapAccumL doAdvance 0 . map (* tblRateFactor)
  where
    tblSizeF = fromIntegral $ DVU.length tbl
    tblRateFactor = tblSizeF / sampleRateF

    doAdvance :: Float -> Float -> (Float, Sample)
    doAdvance !tblPos !tblAdvance = (newTblPos, tbl DVU.! floor tblPos)
      where
        newTblPos = (tblPos + tblAdvance) `mod'` tblSizeF

noteEnvel :: Time -> [Loudness]
noteEnvel attackHoldDur = sampleLinear
    [ (0, attackUpDur)
    , (1, attackDownDur)
    , (0.75, holdDur)
    , (0.75, fadeDur)
    ] 0
  where
    attackDur = min 0.1 attackHoldDur
    attackUpDur = attackDur / 2
    attackDownDur = attackUpDur
    holdDur = attackHoldDur - attackDur
    fadeDur = 0.25
