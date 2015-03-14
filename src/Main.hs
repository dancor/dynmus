{-# LANGUAGE BangPatterns #-}

import Data.List
import Data.List.Split
--import FRP.Netwire
--import Prelude hiding ((.), id)
import Prelude

import Data.WAVE

import Basic
--import Note
--import PlayWire
import Portaudio
import SampTbl

{-
main :: IO ()
main = do
    let sound :: Wire (Timed Float ()) () Identity a Float
        sound = for (1 / 440) . integral (-0.5) . 440 --> sound
    withPortaudio $ playWire sound
-}

main :: IO ()
main = 
    {-
    withPortaudio $ mapM_ playSamples $ chunksOf framesPerBuffer myList
    -}
    toWav "out.wav" myList

myEnvelope :: [Float]
myEnvelope =
    [fromIntegral x / fromIntegral onsetTime | x <- [1 .. onsetTime]] ++
    replicate midTime 1 ++
    [fromIntegral x / fromIntegral fallTime | x <- reverse [1 .. fallTime]]
  where
    totalTime = sampleRate * 2
    onsetTime = sampleRate `div` 5
    midTime = totalTime - onsetTime - fallTime
    fallTime = sampleRate `div` 8

{-
uncons :: [a] -> (a, [a])
uncons (x:xs) = (x, xs)
uncons [] = error "uncons: []"

proportions :: [Float] -> [[Float]] -> [Float]
proportions coeffs = proportions2 (map (/ s) coeffs) where s = sum coeffs

proportions2 :: [Float] -> [[Float]] -> [Float]
proportions2 _ ([]:_)  = []
proportions2 !coeffs theLists = 
    sum (zipWith (*) coeffs theHeads) : proportions2 coeffs theTails
  where
    (theHeads, theTails) = unzip $ map uncons theLists

myBoop :: [Float] -> [Float]
myBoop coeffs = zipWith (*) myEnvelope $ proportions coeffs
    [ realizeFreqs sin10k $ replicate sampleRate 440
    , realizeFreqs sin10k $ replicate sampleRate (440 * 2)
    , realizeFreqs sin10k $ replicate sampleRate (440 * 3)
    , realizeFreqs sin10k $ replicate sampleRate (440 * 4)
    , realizeFreqs sin10k $ replicate sampleRate (440 * 5)
    , realizeFreqs sin10k $ replicate sampleRate (440 * 6)
    , realizeFreqs sin10k $ replicate sampleRate (440 * 7)
    , realizeFreqs sin10k $ replicate sampleRate (440 * 8)
    ]
-}

toWav :: String -> [Float] -> IO ()
toWav f floatSamples = putWAVEFile f $
    WAVE (WAVEHeader 1 sampleRate 32 Nothing) $
    map (:[]) $ map floatToInt32 floatSamples
  where
    floatToInt32 = round . (* 2147483647)

myList :: [Sample]
myList = map (* 0.5) $
    {-
     ++
    replicate 1000 0
    -}
    concatMap myBoop
    [
      [1, 0, 0, 0, 0, 0, 0, 0]
    , [1, 1, 1, 1, 0, 0, 0, 0]
    , [3, 1, 1, 1, 0, 0, 0, 0]
    {-
    , [1, 0, 0, 0, 1, 1, 1, 1]
    , [5, 3, 3, 3, 1, 1, 1, 1]
    -}
    -- [1, 2, 3, 4]
    -- [4, 3, 2, 1]
    -- [1, 0, 0, 0]
    ] ++ replicate 1000 0
