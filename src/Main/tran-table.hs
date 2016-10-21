{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Monoid
import qualified Data.Vector as Vec
import Haskore.Basic.Pitch
--import Text.Printf

import Chord
import Cl
import Hexachord
import Named
import Numbered

data Rank = Low | Medium | High deriving (Eq, Ord, Show)

data MyMode = MyMode
    { mCl   :: !Cl
    , _mNum  :: !Int
    , mName :: !String
    , mMode :: !Mode
    , _mRank :: !Rank
    } deriving Show

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

{-
myModes :: Vec.Vector MyMode
myModes = Vec.fromList $
    [ makeMyMode cl c
    | c <- [hNemne, hNamni, hNiman, hMano, hNom]
    , cl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ]
-}
{-
    [ makeMyMode cl h
    | h <- [hMatnem, hManetam]
    , cl <- [C]
    ]
-}

makeMyMode :: Class -> Nnmq -> MyMode
makeMyMode cla (Numbered n (Named name mq)) =
    MyMode cl n name (modeAt mq cl) High
  where
    cl = intToCl $ classToInt cla

data TranSmoothness
  = TranSmoothness
  { _tSDistSum :: Int
  , _tSHeldNotes :: Int
  , _tSDistSqrSum :: Int
  } deriving Eq

instance Ord TranSmoothness where
  TranSmoothness d1 h1 s1 `compare` TranSmoothness d2 h2 s2 =
    case d2 `compare` d1 of
      LT -> LT
      GT -> GT
      EQ -> case h1 `compare` h2 of
        LT -> LT
        GT -> GT
        EQ -> s2 `compare` s1

instance Show TranSmoothness where
  show (TranSmoothness d h s) =
    "dist" <> show d <> ",held" <> show h <> ",sqr" <> show s

tranSmoothness :: [Cl] -> [Cl] -> TranSmoothness 
tranSmoothness a b = TranSmoothness
    (sum dists)
    (length . filter id $ zipWith (==) a b)
    (sum $ map (\x -> x * x) dists)
  where
    dists = zipWith clDist a b

showCls :: [Cl] -> String
showCls = intercalate " " . map clFwStr

calcTran :: MyMode -> MyMode -> [String]
calcTran a b = if mName a /= mName b || mCl b `clMinus` mCl a >= 0
    then 
      [ ""
      , show (mCl a) <> mName a <> " -> " <> show (mCl b) <> mName b <> ": " <>
        show maxSmoothness
      , showCls aList
      , replicate 17 '-'
      ] ++
      map showCls bestTrans
    else []
  where
    maxSmoothness = maximum . map (tranSmoothness aList) $ permutations bList
    bestTrans = filter ((== maxSmoothness) . tranSmoothness aList) $
        permutations bList
    aList = Vec.toList $ mMode a
    bList = Vec.toList $ mMode b

{-
hexColor :: Int -> Int -> Int -> String
hexColor r g b = '#' : printf "%02x%02x%02x" r g b
-}

goOnModes :: Vec.Vector MyMode -> IO ()
goOnModes modes = do
    let maxI = Vec.length modes - 1
        trans =
            concat
            [ concat
              [ calcTran a b
              | j <- [i + 1 .. maxI]
              , let b = modes Vec.! j
              ]
            | i <- [0 .. maxI]
            , let a = modes Vec.! i
            , mCl a == Cl 0
            ]
    mapM_ putStrLn trans

main :: IO ()
main = goOnModes allModes
