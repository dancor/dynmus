module Main where

import Control.Wire
import qualified Data.Vector as Vec
import qualified FRP.Yampa as Y
import Prelude hiding ((.), id)
import System.Environment

import PlaySig

freq :: Double
freq = 440

volume :: Double
volume = 0.8

-- Yampa sine wave calling (sin).
--
-- Plays smoothly and correct tone.  Though the program runs in ~0.6 s
-- instead of the anticipated ~1.0 s.  Changing the (Y.after 1) to
-- (Y.after 2) runs in ~1.6 s.  I guess Yampa is just doing something weird
-- that chews up the first ~0.4 s of execution.
testSigYampa :: Y.SF () (Double, Y.Event ())
testSigYampa =
    (Y.time >>> Y.arr (\ t -> volume * sin (2 * pi * freq * t)))
    &&&
    Y.after 1 ()

-- Sine wave calling (sin).
--
-- I get popping and maybe warbling and the pitch seems low.
-- Program run time is correctly ~1.0 s.
testSig :: WireP () Double
testSig =
    (\ t -> volume * sin (2 * pi * freq * t)) <$> time . for 2
    <|>
    (\ t -> volume * sin (2 * pi * 2 * freq * t)) <$> time . for 3

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
      [] -> playSig testSig
      ["yampa"] -> playSigYampa testSigYampa
      ["table"] -> playSig testSigTable
      ["sawtooth"] -> playSig testSigSawtooth
      _ -> putStrLn "Usage: ./dynmus {yampa|table|sawtooth}"
