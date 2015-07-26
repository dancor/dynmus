module MusCalc where

import Data.Monoid
import qualified Data.Vector as Vec

import LolHaskore
import Util

-- type MyC = V.Vector Int
type MyC = [Int]

myCEmpty :: MyC
myCEmpty = []

-- n + 12 * o >= lowestNoteAllowed2
-- o >= ceil ((lowestNoteAllowed2 - n) / 12)
--
-- n + 12 * o <= highestNoteAllowed2
-- o <= floor ((highestNoteAllowed2 - n) / 12)
calcVoicings :: Absolute -> (Absolute, Absolute) -> (Relative, Relative)
    -> Vec.Vector Int -> [MyC]
calcVoicings bassNote nRange@(lowestNoteAllowed, highestNoteAllowed)
        iRange@(smallestIntervalAllowed, largestIntervalAllowed) v =
    if Vec.null v then [myCEmpty] else computePoss
  where
    computePoss = concat
        [ map (nAbs:) . calcVoicings nAbs nRange iRange $
          Vec.map ((`mod` 12) . (\x -> x - nDiff)) modeLeft
        | (nDiff, modeLeft) <- pullEachElem v
        , let n = (bassNote + nDiff) `mod` 12
        , o <- [
              (lowestNoteAllowed2 - n + 11) `div` 12 ..
              (highestNoteAllowed2 - n) `div` 12]
        , let nAbs = n + 12 * o
        ]
    lowestNoteAllowed2 =
        max (bassNote + smallestIntervalAllowed) lowestNoteAllowed
    highestNoteAllowed2 =
        min (bassNote + largestIntervalAllowed) highestNoteAllowed
