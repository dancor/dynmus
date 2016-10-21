-- The plan:
-- Find nice hexachord progressions that keep the voices in bounds,
-- that don't repeat exact voicings close together.

module Main where

import Control.Monad.Random
import Data.Int
import Data.List

type Note = Int8

showNote :: Note -> String
showNote n = showPitchClass pitchClass ++ show octave
  where (octave, pitchClass) = n `quotRem` 12

type Notes = [Note]

showNotes :: Notes -> String
showNotes = intercalate " " . map showNote

showPitchClass :: Int8 -> String
showPitchClass  0 = " C"
showPitchClass  1 = "C#"
showPitchClass  2 = " D"
showPitchClass  3 = "D#"
showPitchClass  4 = " E"
showPitchClass  5 = " F"
showPitchClass  6 = "F#"
showPitchClass  7 = " G"
showPitchClass  8 = "G#"
showPitchClass  9 = " A"
showPitchClass 10 = "A#"
showPitchClass 11 = " B"
showPitchClass  c = error $ "showPitchClass: " ++ show c

noteC3, noteCs3, noteD3, noteDs3, noteE3, noteF3, noteFs3, noteG3, noteGs3,
  noteA3, noteAs3, noteB3 :: Note
noteC3  = 3 * 12 + 0
noteCs3 = 3 * 12 + 1
noteD3  = 3 * 12 + 2
noteDs3 = 3 * 12 + 3
noteE3  = 3 * 12 + 4
noteF3  = 3 * 12 + 5
noteFs3 = 3 * 12 + 6
noteG3  = 3 * 12 + 7
noteGs3 = 3 * 12 + 8
noteA3  = 3 * 12 + 9
noteAs3 = 3 * 12 + 10
noteB3  = 3 * 12 + 11

noteC4, noteCs4, noteD4, noteDs4, noteE4, noteF4, noteFs4, noteG4, noteGs4,
  noteA4, noteAs4, noteB4 :: Note
noteC4  = 4 * 12 + 0
noteCs4 = 4 * 12 + 1
noteD4  = 4 * 12 + 2
noteDs4 = 4 * 12 + 3
noteE4  = 4 * 12 + 4
noteF4  = 4 * 12 + 5
noteFs4 = 4 * 12 + 6
noteG4  = 4 * 12 + 7
noteGs4 = 4 * 12 + 8
noteA4  = 4 * 12 + 9
noteAs4 = 4 * 12 + 10
noteB4  = 4 * 12 + 11

noteC5, noteCs5, noteD5, noteDs5, noteE5, noteF5, noteFs5, noteG5, noteGs5,
  noteA5, noteAs5, noteB5 :: Note
noteC5  = 5 * 12 + 0
noteCs5 = 5 * 12 + 1
noteD5  = 5 * 12 + 2
noteDs5 = 5 * 12 + 3
noteE5  = 5 * 12 + 4
noteF5  = 5 * 12 + 5
noteFs5 = 5 * 12 + 6
noteG5  = 5 * 12 + 7
noteGs5 = 5 * 12 + 8
noteA5  = 5 * 12 + 9
noteAs5 = 5 * 12 + 10
noteB5  = 5 * 12 + 11

{-
vecRandChoice :: (MonadRandom m) => V.Vector b -> m b
vecRandChoice l = do
    i <- getRandomR (0, V.length l - 1) 
    return $ l V.! i
-}

rotPoss :: [a] -> [[a]]
rotPoss xs = map (take l) . take l . tails $ cycle xs
  where l = length xs

diffs :: [Int8] -> [Int8]
diffs xs = map (`mod` 12) $ zipWith (-) (tail xs) xs

notesMode :: Notes -> [Int8]
notesMode = minimum . map diffs . rotPoss . sort . map (`rem` 12)

isGood :: Notes -> Bool
isGood ns = case notesMode ns of
    [1,2,2,3,2] -> True
    _ -> False

-- There are 5^6 - 1 (~16k) ways the notes can move.
-- Assumes ns has length 6.
growProgression :: Note -> Note -> Notes -> IO [Note]
growProgression minN maxN ns = do
    let f n = getRandomR (max minN (n - 2), min maxN (n + 2))
    r <- mapM f ns
    if isGood r then return $ sort r else growProgression minN maxN ns

growN :: Int -> (a -> IO a) -> a -> IO [a]
growN 1 _ xs = return [xs]
growN d f xs = do
    cur <- f xs
    rest <- growN (d - 1) f cur
    return $ xs:rest

main :: IO ()
main = do
    r <- growN 10 (growProgression noteC4 noteB5)
        [noteC4, noteE4, noteG4, noteB4, noteD5, noteA5]
        -- [noteC3, noteG3, noteD4, noteA4, noteE5, noteB5]
    mapM_ (putStrLn . showNotes) r
