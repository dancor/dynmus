#include <h>

import Cl
import Chord
import Named
import Util

regularity :: ModeQ -> Ratio Int
regularity mq@(ModeQ v) = sum . map invScore $ mqInversions mq
  where
    regIntvl :: Ratio Int
    regIntvl = 12 % Vec.length v
    invScore :: Vec.Vector Int -> Ratio Int
    invScore = sum . zipWith (\x y -> abs $ x - fromIntegral y)
        (iterate (* regIntvl) regIntvl) . Vec.toList . Vec.scanl1 (*)

main :: IO ()
main = putStr $ unlines chords5

chords6 :: [String]
chords6 =
    map (\(Named n x) ->
        padr 9 ' ' n <> " " <> intercalate " " (map show $ Set.toList x)) $
    map (`nmqAtCl` (Cl 0)) . sortBy (compare `on` regularity . unName) $
    genNChords 6

chords5 :: [String]
chords5 =
    map (\(Named n x) ->
        padr 7 ' ' n <> " " <> intercalate " " (map show $ Set.toList x)) $
    map (`nmqAtCl` (Cl 0)) . sortBy (compare `on` regularity . unName) $
    genNChords 5
