module Main where

import Control.Wire
import qualified Data.Vector as Vec
import Prelude hiding ((.), id)
import System.Environment

import PlaySig

freq :: Double
freq = 440

volume :: Double
volume = 0.8

-- Sine wave calling (sin).
testSig :: WireP () Double
testSig =
    (\ t -> volume * sin (2 * pi * freq * t)) <$> time . for 1
    -- <|>
    -- pure 0 . for 3

decPart :: Double -> Double
decPart x = snd (properFraction x :: (Int, Double))

sineTableSize :: Int
sineTableSize = 200

sineTable :: Vec.Vector Double
sineTable = Vec.fromList $
    map (\i -> sin $ (fromIntegral i / fromIntegral sineTableSize) * pi * 2)
        [0 .. sineTableSize - 1]

-- Sine wave with a sine table.
--
-- I get popping and maybe warbling and the pitch seems low.
-- Program run time is correctly ~1.0 s.
testSigTable :: WireP () Double
testSigTable = (
    (\ t -> volume * (sineTable Vec.!
        (floor (freq * fromIntegral sineTableSize * t) `mod` sineTableSize))
    ) <$> time) . for 1

-- Sawtooth wave.
--
-- I get popping and maybe warbling and the pitch seems low.
-- Program run time is correctly ~1.0 s.
testSigSawtooth :: WireP () Double
testSigSawtooth = (((volume *) . decPart . (* freq)) <$> time) . for 1

main :: IO ()
main = do
    args <- getArgs
    case args of
      [] -> playSig $ testSig
      ["table"] -> playSig testSigTable
      ["sawtooth"] -> playSig testSigSawtooth
      _ -> putStrLn "Usage: ./dynmus {yampa|table|sawtooth}"
