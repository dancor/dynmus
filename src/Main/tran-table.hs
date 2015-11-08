{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.List
import Data.Monoid
import qualified Data.Set as Set
--import qualified Data.Text.Lazy as DTL
--import qualified Data.Text.Lazy.IO as DTLI
import qualified Data.Vector as Vec
import Haskore.Basic.Pitch
import Text.Printf

import Chord
--import Cl
import Hexachord
import Named
import Numbered

{-
data TranStats = TranStats
    { tChangedNoteNum :: Int
    , tDist :: Int
    }
-}

type TranStats = (Int, Int)

data Rank = Low | Medium | High deriving (Eq, Ord, Show)

data MyMode = MyMode
    { mCl   :: !Cl
    , mNum  :: !Int
    , mName :: !String
    , mMode :: !Mode
    , _mRank :: !Rank
    } deriving Show

type ClSet = Set.Set Cl

clDist :: Cl -> Cl -> Relative
clDist a b = min ((aI - bI) `mod` 12) ((bI - aI) `mod` 12)
  where
    aI = classToInt a
    bI = classToInt b

-- The sum of the minimum distances the notes would have to move to remain
-- as six voices. Now: clSetTranDist a b == clSetTranDist b a
clSetTranDist :: ClSet -> ClSet -> Relative
clSetTranDist a = minimum . map (sum . zipWith clDist (Set.toList a)) .
    permutations . Set.toList 


allModes :: Vec.Vector MyMode
allModes = Vec.fromList $
    [ makeMyMode cl c
    | c <- [cNu]
    , cl <- [C, Cs]
    ] ++
    [ makeMyMode cl c
    | c <- [cNemne, cNamni, cNiman, cMano, cNom]
    , cl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ] ++
    [ makeMyMode cl c
    | c <- [cMantman, cNamtanam]
    , cl <- [C, Cs, D, Ds, E, F]
    ] ++
    [ makeMyMode cl c
    | c <- [cMantnam, cNamnatam, cNatmanam, cManetam,
      cMatnem, cNatmen, cNatname, cTamnaman, cTanmanam, cNetme, cTamnem,
      cTanmen, cTamene, cTaneme, cNatnarn, cNatner, cNatrane, cNetnar, cNetran,
      cTanern, cTanrane, cNitar, cTanir, cTrani]
    , cl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ]

statsForTran :: ClSet -> ClSet -> TranStats
statsForTran a b =
    ( 6 - Set.size (Set.intersection a b)
    , clSetTranDist a b
    )

makeMyMode :: Cl -> Nnmq -> MyMode
makeMyMode cl (Numbered n (Named name mq)) =
    MyMode cl n name (modeAt mq cl) High

calcTran :: MyMode -> MyMode -> ((Int, Int), (TranStats, Cl))
calcTran a b =
    ( (mNum a, mNum b)
    , ( statsForTran
        (Set.fromList . Vec.toList $ mMode a)
        (Set.fromList . Vec.toList $ mMode b)
      , mCl b
      )
    )

hexColor :: Int -> Int -> Int -> String
hexColor r g b = '#' : printf "%02x%02x%02x" r g b

main :: IO ()
main = do
    let maxI = Vec.length allModes - 1
        trans =
            filter ((== (3, 4)) . fst . snd) $ concat
            [ map (\xs@((x1,(x2,_)):_) -> (x1, (x2, map (snd . snd) xs))) .
              groupBy (\(x1,(x2,_)) (y1,(y2,_)) -> x1 == y1 && x2 == y2) $
              sortBy ((compare `on` fst . snd) `mappend` (compare `on` fst))
              [ calcTran a b
              | j <- [i + 1 .. maxI]
              , let b = allModes Vec.! j
              ]
            | i <- [0 .. maxI]
            , let a = allModes Vec.! i
            , mCl a == C
            ]
    -- putStr . unlines . map show . sortBy (compare `on` snd) $ concat
    putStrLn "digraph lol {"
    putStrLn "    node [shape=plaintext];"
    putStr . unlines . map (\n ->
        let Named name mq = unNumber $ hexachords !! n
        in
        "    " ++ show n ++ " [label=" ++ show name ++
        -- " fonsize=8" ++
        " fontcolor=" ++ show (hexColor ((n * 128) `div` 32) 128 0) ++ "];"
        ) .
        Set.toList . Set.fromList $ concatMap (\((a,b), _) -> [a,b]) trans
    putStr . unlines $
        map (\((a,b),(_,cls)) ->
            let label = intercalate "," $ map (($ "") . classFormat) cls
                node = show a ++ "," ++ show b ++ "," ++ label
            in
            "    " ++ show node ++ " [label=" ++ show label ++ 
            " fontsize=8" ++
            " fontcolor=purple];\n" ++
            "    " ++ show a ++ " -> " ++ show node ++ " -> " ++ show b ++ ";"
            --"    " ++ show a ++ " -> " ++ show b ++ " [" ++
            -- "color=gray fontsize=8 fontcolor=orange labelangle=1 labeldistance=3 headlabel=\"" ++
            --intercalate "," (map (($ "") . classFormat) cls) ++ "\"];"
            )
            trans
        --map show .
    putStrLn "}"

{-
onNn :: (a -> b) -> Numbered (Named a) -> Numbered (Named b)
onNn f = onNumbered (onNamed f)
-}

