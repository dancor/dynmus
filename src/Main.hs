{-# LANGUAGE BangPatterns #-}

import Control.Applicative
import Control.Monad
import Data.Function
import Data.List
import Data.List.Split
--import Debug.Trace
import Haskore.Basic.Pitch as Pitch
import Numeric
import System.Environment
import System.Random

import Chord
import LolHaskore
import MusCalc
import Note

{-
cFromList = id
cToList = id

vchoiceIO :: V.Vector a -> IO a
vchoiceIO xs = do
    i <- randomRIO (1, V.length xs)
    return $ xs V.! (i - 1)

adjDiffs :: Num a => [a] -> [a]
adjDiffs v = zipWith (-) (tail v) v

smallestDiff :: MyC -> Int
smallestDiff = minimum . map abs . adjDiffs . sort . cToList

chooseChordIO :: IO (Int, (ChordQual, String))
chooseChordIO = do
    i <- randomRIO (0, length namedHexachords - 1)
    return $ (i, namedHexachords !! i)
-}

choiceIO :: [a] -> IO a
choiceIO xs = do
    i <- randomRIO (1, length xs)
    return $ xs !! (i - 1)

noteDistFromChord :: Int -> MyC -> Int
noteDistFromChord n ns2 = minimum [abs (n - n2) | n2 <- ns2]

chordDist :: MyC -> MyC -> Int
chordDist ns1 ns2 = sum [noteDistFromChord n ns2 ^ (2 :: Int) | n <- ns1]

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
    let lineLen = 4 :: Int
        jumpMax = 4 :: Int
        nextPitch p = randomRIO
            ( max basePitchMin (p - jumpMax)
            , min basePitchMax (p + jumpMax)
            )
        loop n pPrev = if n <= 1 then return [] else do
            p <- nextPitch pPrev
            (p:) <$> loop (n - 1) p
    pitch1 <- randomRIO (basePitchMin, basePitchMax)
    (pitch1:) <$> loop lineLen pitch1

absToMus :: Absolute -> Mus
absToMus x = nAbs x qn

nAbs :: Absolute -> Dur -> Mus
nAbs x d = note (Pitch.fromInt x) d na

-- In Haskore 0 is low C (C3). This is odd;
-- logically it should be C0 or middle C (C4).
showAbs :: Absolute -> String
showAbs = (\(o, n) -> show n ++ show (o + 3)) . Pitch.fromInt

padr :: Int -> a -> [a] -> [a]
padr n c cs = cs ++ replicate (n - length cs) c

myArpeg :: [Int] -> Mus
myArpeg [n1, n2, n3, n4, n5, n6] = chord
    [                    nAbs n1 (12 %+ 48)
    , rest (1 %+ 48) +:+ nAbs n2 (11 %+ 48)
    , rest (2 %+ 48) +:+ nAbs n3 (10 %+ 48)
    , rest (3 %+ 48) +:+ nAbs n4 (9 %+ 48)
    , rest (4 %+ 48) +:+ nAbs n5 (8 %+ 48)
    , rest (5 %+ 48) +:+ nAbs n6 (7 %+ 48)
    ]
    {-
    [                    nAbs n1 (6 %+ 24)
    , rest (1 %+ 24) +:+ nAbs n2 (5 %+ 24)
    , rest (2 %+ 24) +:+ nAbs n3 (4 %+ 24)
    , rest (3 %+ 24) +:+ nAbs n4 (3 %+ 24)
    , rest (4 %+ 24) +:+ nAbs n5 (2 %+ 24)
    , rest (5 %+ 24) +:+ nAbs n6 (1 %+ 24)
    ]
    [                    nAbs n1 (6 %+ 48) +:+
                         nAbs n1 (6 %+ 48)
    , rest (1 %+ 48) +:+ nAbs n2 (5 %+ 48) +:+
      rest (1 %+ 48) +:+ nAbs n2 (5 %+ 48) 
    , rest (2 %+ 48) +:+ nAbs n3 (4 %+ 48) +:+
      rest (2 %+ 48) +:+ nAbs n3 (4 %+ 48)

    , rest (3 %+ 48) +:+ nAbs n4 (6 %+ 48) +:+
                         nAbs n4 (3 %+ 48)
    , rest (4 %+ 48) +:+ nAbs n5 (5 %+ 48) +:+
      rest (1 %+ 48) +:+ nAbs n5 (2 %+ 48)
    , rest (5 %+ 48) +:+ nAbs n6 (4 %+ 48) +:+
      rest (2 %+ 48) +:+ nAbs n6 (1 %+ 48) 
    ]
    -}
myArpeg _ = error "myArpeg: takes 6 notes"

basePitchMin, basePitchMax :: Int
(basePitchMin, basePitchMax) = basePitchRange

basePitchRange, voicingPitchRange, voicingIntervalRange :: (Int, Int)

basePitchRange = (noteAbs nGb2, noteAbs nGb3)

voicingPitchRange = (noteAbs nGb2, noteAbs nC5)

voicingIntervalRange = (2, 14)

main :: IO ()
main = do
    args <- getArgs
    seed <- case args of
      [] -> (`mod` 0xffffffff) <$> randomIO
      [seedStr] -> case readHex seedStr of
        [(seed, "")] -> return seed
        _ -> error "usage: Could not parse random seed hex string."
      _ -> error "usage: dynmus (random-seed-hex-string)"
    putStrLn $ "Random seed: " ++ showHex seed ""
    putStrLn ""
    setStdGen $ mkStdGen seed

    baseline <- chooseBaseline
    chordIs <- replicateM 4 $ randomRIO (0, length namedHexachords - 1)
    let (chordQs, names) = unzip $ map (namedHexachords !!) chordIs
        modes = map myChooseMode chordQs
        voicingLists = zipWith
            (\bassNote -> map (bassNote :) .
                concatMap (calcVoicings bassNote voicingPitchRange
                voicingIntervalRange) . allInversions)
            baseline modes

    zipWithM_ (\n name -> putStrLn $ padr 3 ' ' (showAbs n) ++ " " ++ name)
        baseline names
    putStrLn $ "Voicing options: " ++ show (map length voicingLists)
    --voicings <- mapM choiceIO voicingLists
    voicings <- pickCards $ concat $ replicate 4 voicingLists
    putStrLn ""
    putStr . unlines . intercalate [""] . chunksOf 4 $
        map (intercalate " " . map (padr 3 ' ' . showAbs)) voicings

    playPiano $
        line (map absToMus baseline) +:+
        line (map absToMus baseline) +:+
        line (map myArpeg voicings) +:+
        line (map absToMus baseline) +:+
        line (map (flip nAbs hn) $ take 1 baseline)
        --line (map (chord . map absToMus) voicings)
