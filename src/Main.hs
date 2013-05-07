module Main where

import Control.Wire
import qualified Data.Vector as Vec
import qualified FRP.Yampa as Y
import Prelude hiding ((.), id)
import System.Environment

import PlaySig

freq :: Double
freq = 880

volume :: Double
volume = 0.8

-- Yampa sine wave calling (sin).
testSigYampa :: Y.SF () (Double, Y.Event ())
testSigYampa =
    (Y.time >>> Y.arr (\ t -> volume * sin (2 * pi * freq * t)))
    &&&
    Y.after 1 ()

-- Sine wave calling (sin).
testSig :: WireP () Double
testSig = (\ t -> volume * sin (2 * pi * freq * t)) <$> time . for 1

decPart :: Double -> Double
decPart x = snd (properFraction x :: (Int, Double))

sineTableSize :: Int
sineTableSize = 200

sineTable :: Vec.Vector Double
sineTable = Vec.fromList $
    map (\i -> sin $ (fromIntegral i / fromIntegral sineTableSize) * pi * 2)
        [0 .. sineTableSize - 1]

-- Sine wave with a sine table.
testSigTable :: WireP () Double
testSigTable = (
    (\ t -> volume * (sineTable Vec.!
        (floor (freq * fromIntegral sineTableSize * t) `mod` sineTableSize))
    ) <$> time) . for 1

-- Sawtooth wave. 
testSigSawtooth :: WireP () Double
testSigSawtooth = ((decPart . (* freq)) <$> time) . for 1

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> playSig testSig
      ["yampa"] -> playSigYampa testSigYampa
      ["table"] -> playSig testSigTable
      ["sawtooth"] -> playSig testSigSawtooth
      _ -> putStrLn "Usage: ./dynmus {yampa|table|sawtooth}"
