#include <h>

import Cl
import Chord
import Named
import Util

type Regularity = [Ratio Int]

invScore :: Vec.Vector Int -> Regularity
invScore v = zipWith (\x y -> abs (x - fromIntegral y))
    (iterate (* regIntvl) regIntvl) . Vec.toList $ Vec.scanl1 (*) v
  where
    regIntvl :: Ratio Int
    regIntvl = 12 % Vec.length v

regularity :: ModeQ -> Irregularity
regularity (ModeQ v) = (unders, overs, unevenness) where
    vDiff = Vec.map ((subtract regInvtl) . fromIntegral) v
    unders = Vec.sort $ Vec.filter (< 0) vDiff
    overs = Vec.sort $ Vec.filter (> 0) vDiff
    -- map sum . transpose . map invScore $ vRotPoss v
--regularity mq@(ModeQ v) = invScore v

main :: IO ()
main = putStr . unlines $ chords 5 7
--main = putStr . unlines $ chords 6 9

chords :: Int -> Int -> [String]
chords noteNum padNum =
    map (\((r, ModeQ x), Named n y) ->
        padr padNum ' ' n <> " " <> concatMap show (Vec.toList x) <> " " <>
        -- show (map realToFrac r) <> " " <>
        intercalate " " (map show $ Set.toList y)) .
    sortBy (compare `on` fst . fst) .
    map (first (\x -> (regularity x, x))) .
    map (\x -> (unName x, nmqAtCl x (Cl 0))) $
    genNChords noteNum
