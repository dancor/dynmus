{-# LANGUAGE BangPatterns #-}

--import FRP.Netwire
--import Prelude hiding ((.), id)

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.List.Utils
import Data.Maybe
import qualified Data.Vector as V
import Data.WAVE
import Debug.Trace
import Haskore.Basic.Pitch as Pitch
import qualified Sound.Font as SF
import System.Environment
import System.Random

import Chord
import Note
import Portaudio
import SampTbl
import LolHaskore
import MusCalc

chooseChordIO :: IO (Int, (ChordQual, String))
chooseChordIO = do
    i <- randomRIO (0, length namedHexachords - 1)
    return $ (i, namedHexachords !! i)

cFromList = id
cToList = id

{-
vchoiceIO :: V.Vector a -> IO a
vchoiceIO xs = do
    i <- randomRIO (1, V.length xs)
    return $ xs V.! (i - 1)
-}
choiceIO :: [a] -> IO a
choiceIO xs = do
    i <- randomRIO (1, length xs)
    return $ xs !! (i - 1)

adjDiffs :: Num a => [a] -> [a]
adjDiffs v = zipWith (-) (tail v) v

smallestDiff :: MyC -> Int
smallestDiff = minimum . map abs . adjDiffs . sort . cToList

noteDistFromChord :: Int -> MyC -> Int
noteDistFromChord n ns2 = minimum [abs (n - n2) | n2 <- ns2]

chordDist :: MyC -> MyC -> Int
chordDist ns1 ns2 = sum [noteDistFromChord n ns2 | n <- ns1]

pickNextCard :: MyC -> [MyC] -> IO MyC
-- pickNextCard prev curs = choiceIO $ filter (chordDist) curs
pickNextCard prev curs = choiceIO $ take 10 $
    sortBy (compare `on` chordDist prev) $
    filter (/= prev) curs

pickCards :: [[MyC]] -> IO [MyC]
pickCards (hands1:handsRest) = do
    let loop _ [] = return []
        loop prev (h:hRest) = do
            cur <- pickNextCard prev h
            r <- loop cur hRest
            return $ cur : r
    hand1 <- choiceIO hands1
    (hand1 :) <$> loop hand1 handsRest
pickCards [] = error "pickCards: no hands"

chooseBaseline :: IO [Absolute]
chooseBaseline = do
    let lineLen = 4
        absPitchMin = -24
        absPitchMax = 0
        jumpMax = 4
        nextPitch p = randomRIO
            ( max absPitchMin (p - jumpMax)
            , min absPitchMax (p + jumpMax)
            )
        loop n pPrev = if n <= 1 then return [] else do
            p <- nextPitch pPrev
            (p:) <$> loop (n - 1) p
    pitch1 <- randomRIO (absPitchMin, absPitchMax)
    (pitch1:) <$> loop lineLen pitch1

intToMus :: Absolute -> Mus
intToMus = (\x -> note x qn na) . Pitch.fromInt

main :: IO ()
main = do
    args <- getArgs
    chordIs <- case args of
        [] -> replicateM 4 $ randomRIO (0, length namedHexachords - 1)
        [chordIStr] -> return $ replicate 4 (read chordIStr - 1)
        _ -> error "usage"
    baseline <- chooseBaseline
    let (chordQs, names) = unzip $ map (namedHexachords !!) chordIs
        modes = map myChooseMode chordQs
        voicingLists = zipWith
            (\bassNote -> calcVoicings bassNote (-24, 36) (3, 14))
            baseline modes
    zipWithM_ (\n name -> putStrLn $ show n ++ " " ++ name) baseline names
    print $ map length voicingLists
    --voicings <- mapM choiceIO voicingLists
    voicings <- pickCards voicingLists
    print voicings

    playPiano $
        line (map intToMus baseline) +:+
        line (map intToMus baseline) +:+
        line (
            zipWith (\n v -> chord $ map intToMus (n:v)) baseline voicings
            ) +:+
        line (
            zipWith (\n v -> chord $ map intToMus (n:v)) baseline voicings
            )
