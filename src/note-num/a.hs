import Control.Arrow
import Data.Function
import Data.List

data Cents = Cents Int

toCents :: Double -> Int
toCents = floor . (* 1200)

intervalsToCheck :: [(String, Double)]
intervalsToCheck =
    [ ("P5", 3 / 2)
    , ("M3", 5 / 4)
    , ("m3", 6 / 5)
    , ("76", 7 / 6)
    , ("87", 8 / 7)
    , ("M2", 9 / 8)
    ]

noteFreqs :: Int -> [(Int, Double)]
noteFreqs n =
    [(i, 2 ** (fromIntegral i / fromIntegral n)) | i <- [1 .. n - 1]]

noteDiffs :: Double -> [(Int, Double)] -> [(Int, Double)]
noteDiffs intvl = map (second (abs . (intvl -)))

bestNote :: Double -> [(Int, Double)] -> (Int, Double)
bestNote intvl = minimumBy (compare `on` snd) . noteDiffs intvl

scaleInfoUpTo :: Int -> [(Int, [(String, (Int, Double))])]
scaleInfoUpTo maxN =
    [ ( n
      , [ (intvlName, bestNote intvl notes)
        | (intvlName, intvl) <- intervalsToCheck
        ]
      )
    | n <- [2 .. maxN]
    , let notes = noteFreqs n
    ]

showScaleInfo :: (Int, [(String, (Int, Double))]) -> String
showScaleInfo (n, infos) = intercalate "\t" $
    [show n ++ ":", show . toCents . sum $ map (snd . snd) infos] ++
    [ intvlName ++ ": " ++ show (steps, toCents freq)
    | (intvlName, (steps, freq)) <- infos
    ]

main :: IO ()
main = do
    -- let notes = noteFreqs 12
    -- putStr . unlines $ map show notes
    putStr $ unlines $ map showScaleInfo $
       filter (all ((< 20) . toCents . snd . snd) . snd) $ scaleInfoUpTo 81
