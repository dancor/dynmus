module MusCalc where

import Debug.Trace

import Chord
import LolHaskore

-- type MyC = V.Vector Int
type MyC = [Int]

myCEmpty :: MyC
myCEmpty = []

pullEachElem :: [a] -> [(a, [a])]
pullEachElem = pullEachElemA []
  where
    pullEachElemA _ [] = []
    pullEachElemA did (x:xs) = (x, did ++ xs) : pullEachElemA (did ++ [x]) xs

-- n + 12 * o >= lowestNoteAllowed2
-- o >= ceil ((lowestNoteAllowed2 - n) / 12)
--
-- n + 12 * o <= highestNoteAllowed2
-- o <= floor ((highestNoteAllowed2 - n) / 12)
calcVoicings :: Absolute -> (Absolute, Absolute) -> (Relative, Relative)
    -> ModeQual -> [MyC]
calcVoicings _ _ _ [] = [myCEmpty]
calcVoicings bassNote nRange@(lowestNoteAllowed, highestNoteAllowed)
        iRange@(smallestIntervalAllowed, largestIntervalAllowed) mode =
    concat
    [
      map (nAbs:) $ calcVoicings nAbs nRange iRange $
      map ((`mod` 12) . (\x -> x - nDiff)) modeLeft
    | (nDiff, modeLeft) <- pullEachElem mode
    , let n = (bassNote + nDiff) `mod` 12
    , o <- [
          (lowestNoteAllowed2 - n + 11) `div` 12 ..
          (highestNoteAllowed2 - n) `div` 12]
    , let nAbs = n + 12 * o
    ]
  where
    lowestNoteAllowed2 =
        max (bassNote + smallestIntervalAllowed) lowestNoteAllowed
    highestNoteAllowed2 =
        min (bassNote + largestIntervalAllowed) highestNoteAllowed
