#include <h>

import Chord
import Named

isGoodName :: String -> Bool
--isGoodName x = x `elem` ["nu", "mano", "namni", "nemne", "nimna", "nom"]
isGoodName x = x `elem` ["mano", "namni", "nemne", "nimna", "nom"]

main :: IO ()
main = mapM_ print good8

good8 :: [Named Int]
good8 = filter ((>= 4) . unName) $ map
    (onNamed (length . filter isGoodName .
        map (nameMq . ltChooseMode . ChordQ . unMQ) .
        concatMap subchords . subchords))
    (genNChords 8)

good7 :: [Named Int]
good7 = filter ((>= 4) . unName) $ map
    (onNamed (length . filter isGoodName .
        map (nameMq . ltChooseMode . ChordQ . unMQ) .
        subchords))
    (genNChords 7)
