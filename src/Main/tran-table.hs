{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Monoid
import qualified Data.Vector as Vec
import Data.Word
import qualified Euterpea.Music as E
import Euterpea.Music (PitchClass(..))
--import Text.Printf

import Chord
import PiCl
import Hexachord
import Named
import Numbered

data Rank = Low | Medium | High deriving (Eq, Ord, Show)

data MyMode = MyMode
    { mPiCl   :: !PiCl
    , _mNum  :: !Int
    , mName :: !String
    , mMode :: !CMode
    , _mRank :: !Rank
    } deriving Show

subtrimonicModes :: Vec.Vector MyMode
subtrimonicModes = Vec.fromList $
    [ makeMyMode piCl c
    | c <- [hNemne]
    , piCl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ]
    {-
    [ makeMyMode piCl c
    | c <- [hNemne, hNamni, hNiman, hMano, hNom]
    , piCl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ]
    ++
    [ makeMyMode piCl c
    | c <- [hNu]
    , piCl <- [C, Cs]
    ] ++
    [ makeMyMode piCl c
    | c <- [hManetam, hMatnem, hNitar, hTamnem, hNetnar, hNetran,
      hTamnaman, hTanmanam, hNatner, hNatrane, hTamene, hTaneme,
      hMantnam, hNamnatam, hNatmanam, hNetme, hNatmen, hNatname,
      hNatnarn, hTanmen, hTanir, hTrani, hTanern, hTanrane]
    , piCl <- [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]
    ] ++
    [ makeMyMode piCl c
    | c <- [hMantman, hNamtanam]
    , piCl <- [C, Cs, D, Ds, E, F]
    ]
    -}

makeMyMode :: E.PitchClass -> Nnmq -> MyMode
makeMyMode pitchClass (Numbered n (Named name mq)) =
    MyMode piCl n name (modeAt mq piCl) High
  where
    piCl = intToPiCl $ E.pcToInt pitchClass

-- lower numbers are smoother, earlier numbers matter more
data TranUnsmoothness
  = TranUnsmoothness
  { _tParallelFifths :: Int
  , _tNumJumps :: Int
  , _tSDistSum :: Int
  , _tSHeldNotes :: Int
  , _tSDistSqrSum :: Int
  } deriving (Eq, Ord)

instance Show TranUnsmoothness where
  show (TranUnsmoothness p j d h s) = "para" <> show p <>
    ",jumps" <> show j <> ",dist" <> show d <>
    ",move" <> show (6 - h) <> ",sqr" <> show s

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs (x:xs) = map ((,) x) xs ++ pairs xs

isParallelFifth ((from1,to1),(from2,to2)) =
     from1 /= to1 && fromDiff == toDiff && isFifthy toDiff
   where
     fromDiff = from2 `piClMinus` from1
     toDiff = to2 `piClMinus` to1

isFifthy :: Int -> Bool
isFifthy (-5) = True
isFifthy 5 = True
--isFifthy 6 = True
isFifthy _ = False

tranUnsmoothness :: [PiCl] -> [PiCl] -> TranUnsmoothness 
tranUnsmoothness a b = TranUnsmoothness
    (length . filter isParallelFifth . pairs $ zip a b)
    (length $ filter (> 2) dists)
    (sum dists)
    (length . filter id $ zipWith (==) a b)
    (sum $ map (\x -> x * x) dists)
  where
    dists = zipWith piClDist a b

showPiCls :: [PiCl] -> String
showPiCls = intercalate " " . map show

calcTran :: MyMode -> MyMode -> [String]
calcTran a b = if mName a /= mName b || mPiCl b `piClMinus` mPiCl a >= 0
    then 
      (\(x:xs) -> x <> " -> " <> show (mPiCl b) <> mName b <> ":\t" <>
        show minUnsmoothness : xs
      ) (map showPiCls bestTrans)
    else []
  where
    minUnsmoothness = minimum . map (tranUnsmoothness aList) $
        permutations bList
    bestTrans = filter ((== minUnsmoothness) . tranUnsmoothness aList) $
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
            [ [ showPiCls (Vec.toList $ mMode a) <> " " <> show (mPiCl a) <>
                mName a
              , replicate 17 '-'
              ] ++
              concat
              [ calcTran a b
              | j <- [i + 1 .. maxI]
              , let b = modes Vec.! j
              ] ++
              [""]
            | i <- [0 .. maxI]
            , let a = modes Vec.! i
            , mPiCl a == PiCl 0
            ]
    mapM_ putStrLn trans

main :: IO ()
main = goOnModes subtrimonicModes
