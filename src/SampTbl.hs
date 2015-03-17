{-# LANGUAGE BangPatterns #-}

module SampTbl where

import qualified Data.Vector.Unboxed as DVU

type Sample = Float

type SampTbl = DVU.Vector Sample

sampleRate :: Int
sampleRate = 44100

sampleRateF :: Float
sampleRateF = fromIntegral sampleRate

sin10k :: SampTbl
sin10k = genSinTbl 10000

genSinTbl :: Int -> SampTbl
genSinTbl !tblSize =
    DVU.generate tblSize (sin . (xChange *) . fromIntegral)
  where
    xChange :: Float
    xChange = 2 * pi / fromIntegral tblSize

genSinsTbl :: Int -> [Float] -> SampTbl
genSinsTbl !tblSize !harmonicAmplitudes = DVU.generate tblSize f
  where
    xChange :: Float
    xChange = 2 * pi / fromIntegral tblSize
    f :: Int -> Float
    f x = sum $ zipWith (*) harmonicAmplitudes
        [sin (xChange * i * fromIntegral x) | i <- [1..8]]

trip10k :: SampTbl
trip10k = genSinsTbl 10000 [1/3, 1/9, 1/9, 1/9, 1/12, 1/12, 1/12, 1/12]
