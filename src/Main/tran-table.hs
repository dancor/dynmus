{-# LANGUAGE OverloadedStrings #-}

import Data.Function
import Data.List
import qualified Data.Set as Set
--import qualified Data.Text.Lazy as DTL
--import qualified Data.Text.Lazy.IO as DTLI
import qualified Data.Vector as Vec
import Haskore.Basic.Pitch

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

data Rank = Low | Medium | High deriving (Eq, Ord)

data MyMode = MyMode
    { mCl   :: !Cl
    , mNum  :: !Int
    , _mName :: !String
    , mMode :: !Mode
    , _mRank :: !Rank
    }


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
    [ MyMode cl n name (modeAt mq cl) High
    | Numbered n (Named name mq) <- [cNu]
    , cl <- [C, Cs]
    ] ++
    [ MyMode cl n name (modeAt mq cl) High
    | Numbered n (Named name mq) <- [cNemne, cNamni, cNiman, cMano, cNom]
    , cl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ] ++
    [ MyMode cl n name (modeAt mq cl) High
    | Numbered n (Named name mq) <- [cMantman, cNamtanam]
    , cl <- [C, Cs, D, Ds, E, F]
    ] ++
    [ MyMode cl n name (modeAt mq cl) High
    | Numbered n (Named name mq) <- [cMantnam, cNamnatam, cNatmanam, cManetam,
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

calcTran :: MyMode -> MyMode -> ((Int, Int), (TranStats, Cl))
calcTran a b =
    ( (mNum a, mNum b)
    , ( statsForTran
        (Set.fromList . Vec.toList $ mMode a)
        (Set.fromList . Vec.toList $ mMode b)
      , mCl b
      )
    )

main :: IO ()
main = do
    let maxI = Vec.length allModes - 1
    -- putStr . unlines . map show . sortBy (compare `on` snd) $ concat
    putStr . unlines . map show . filter ((== (3, 4)) . fst . snd) $ concat
        [ map (\xs@((x1,(x2,_)):_) -> (x1, (x2, map (snd . snd) xs))) .
          groupBy (\(x1,(x2,_)) (y1,(y2,_)) -> x1 == y1 && x2 == y2) $
          sortBy (compare `on` snd)
          [ calcTran a b
          | j <- [i + 1 .. maxI]
          , let b = allModes Vec.! j
          ]
        | i <- [0 .. maxI]
        , let a = allModes Vec.! i
        , mCl a == C
        ]

{-
onNn :: (a -> b) -> Numbered (Named a) -> Numbered (Named b)
onNn f = onNumbered (onNamed f)
-}

