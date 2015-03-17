{-# LANGUAGE BangPatterns #-}

--import FRP.Netwire
--import Prelude hiding ((.), id)

import Data.List.Split
import Data.WAVE
import System.Random

import Chord
import Dynmus
import Note
--import PlayWire
import Portaudio
import SampTbl

type ScaleStep = Int

{-
main :: IO ()
main = do
    let sound :: Wire (Timed Float ()) () Identity a Float
        sound = for (1 / 440) . integral (-0.5) . 440 --> sound
    withPortaudio $ playWire sound
-}

myNote :: NoteNum -> Duration -> Signal
myNote n d = zipWith (*) (noteEnvel d)
    (realizeFreqs trip10k . repeat $ noteNumFreq n)

-- | A pattern to test out the feel of different hexatonic scales.
myPattern :: [ScaleStep]
myPattern =
    [ 1, 3, 5, 3, 1, 3, 5, 3
    , 1, 4, 6, 4, 1, 6, 7, 6
    , 6, 5, 4, 3, 5, 4, 3, 2
    , 1, 1, 1, 1, 1
    ]

realizeScaleStep :: NoteNum -> ModeQual -> ScaleStep -> NoteNum
realizeScaleStep n m s = n + sOctave * 12 + (0:m) !! sIndex
  where
    (sOctave, sIndex) = (s - 1) `divMod` (length m + 1)

main :: IO ()
main = do
    chordI <- randomRIO (0, length hexachords - 1)
    let (chordQ, name) = hexachords !! chordI
        mode = myChooseMode chordQ
    putStrLn ""
    putStrLn name
    putStrLn ""
    withPortaudio . mapM_ playSamples . chunksOf framesPerBuffer .
    {-
    toWav "out.wav" .
    -}
         map (* 0.5) $
         concatMap (flip myNote 0.125 . realizeScaleStep (noteNum nC4) mode)
             myPattern
         {-
         zipWith (\a b -> (a + b) / 2)
             (myNote nEb4 1)
             (myNote nG4 1) ++
         zipWith4 (\a b c d -> (a + b + c + d) / 4)
             (myNote nBb4 1)
             (myNote nD5 1)
             (myNote nF5 1)
             (myNote nAb4 1)
         -}

toWav :: String -> Signal -> IO ()
toWav f floatSamples = putWAVEFile f $
    WAVE (WAVEHeader 1 sampleRate 32 Nothing) $
    map (:[]) $ map floatToInt32 floatSamples
  where
    floatToInt32 = round . (* 2147483647)
