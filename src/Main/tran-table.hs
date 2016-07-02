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
import Cl
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

{-
allModes :: Vec.Vector MyMode
allModes = Vec.fromList $
    [ makeMyMode cl c
    | c <- [hNu]
    , cl <- [C, Cs]
    ] ++
    [ makeMyMode cl c
    | c <- [hNemne, hNamni, hNiman, hMano, hNom]
    , cl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ] ++
    [ makeMyMode cl c
    | c <- [hMantman, hNamtanam]
    , cl <- [C, Cs, D, Ds, E, F]
    ] ++
    [ makeMyMode cl c
    | c <- [hMantnam, hNamnatam, hNatmanam, hManetam,
      hMatnem, hNatmen, hNatname, hTamnaman, hTanmanam, hNetme, hTamnem,
      hTanmen, hTamene, hTaneme, hNatnarn, hNatner, hNatrane, hNetnar, hNetran,
      hTanern, hTanrane, hNitar, hTanir, hTrani]
    , cl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ]
-}

myModes :: Vec.Vector MyMode
myModes = Vec.fromList $
    [ makeMyMode cl c
    | c <- [hNemne, hNamni, hNiman, hMano, hNom]
    , cl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ]
    {-
    [ makeMyMode cl h
    | h <- [hMatnem, hManetam]
    , cl <- [C]
    ]
    -}

statsForTran :: ClSet -> ClSet -> TranStats
statsForTran a b =
    ( 6 - Set.size (Set.intersection a b)
    , clSetTranDist a b
    )

makeMyMode :: Class -> Nnmq -> MyMode
makeMyMode cla (Numbered n (Named name mq)) =
    MyMode cl n name (modeAt mq cl) High
  where
    cl = intToCl $ classToInt cla

calcTran :: MyMode -> MyMode -> ((String, String), (TranStats, Int))
calcTran a b =
    ( (mName a, mName b)
    , ( statsForTran
        (Set.fromList . Vec.toList $ mMode a)
        (Set.fromList . Vec.toList $ mMode b)
      , clToInt $ mCl b
      )
    )

hexColor :: Int -> Int -> Int -> String
hexColor r g b = '#' : printf "%02x%02x%02x" r g b

main :: IO ()
main = do
    let maxI = Vec.length myModes - 1
        trans =
            concat
            [ map (\xs@((x1,(x2,_)):_) -> (x1, (x2, map (snd . snd) xs))) .
              groupBy (\(x1,(x2,_)) (y1,(y2,_)) -> x1 == y1 && x2 == y2) $
              sortBy ((compare `on` fst . snd) `mappend` (compare `on` fst))
              [ calcTran a b
              | j <- [i + 1 .. maxI]
              , let b = myModes Vec.! j
              ]
            | i <- [0 .. maxI]
            , let a = myModes Vec.! i
            , mCl a == Cl 0
            ]
    putStr . unlines . map show . sortBy (compare `on` snd) $ trans
