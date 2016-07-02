{-# LANGUAGE BangPatterns #-}

import Control.Arrow
import Data.Function
import Data.List
import Data.Monoid
import qualified Data.Vector as Vec
import Haskore.Basic.Pitch as HBP

import Cl
import Chord
import Named
import Util

type Freq = Double

type Roughness = Double

data AmpFreq = AmpFreq Double Freq

intvlDissonance :: Int -> Int
intvlDissonance 1 = 100000
intvlDissonance 6 = 10000
intvlDissonance 2 = 1000
intvlDissonance 3 = 100
intvlDissonance 4 = 10
intvlDissonance 5 = 1
{-
intvlDissonance 1 = 55
intvlDissonance 2 = 34
intvlDissonance 3 = 25
intvlDissonance 4 = 24
intvlDissonance 5 = 15
intvlDissonance 6 = 45
-}

intvlsDissonance :: [Int] -> Int
intvlsDissonance = sum . map intvlDissonance

myRough :: [Relative] -> Roughness
myRough = roughness . Vec.concat . map (pianoNote . HBP.intToFreq)

-- Would be nice to calculate amplitudes from a real sample?
pianoNote :: Freq -> Vec.Vector AmpFreq
pianoNote f = Vec.fromList .
    -- zipWith AmpFreq (map (/ 6) [0.4, 0.2, 0.1, 0.1, 0.1, 0.1]) $
    zipWith AmpFreq (map (/ 6) [1/6, 1/6, 1/6, 1/6, 1/6, 1/6]) $
    iterate (+ f) f

roughness :: Vec.Vector AmpFreq -> Roughness
roughness v = sum
    [ sinesRoughness (v Vec.! i) (v Vec.! j)
    | i <- [0 .. lastI], j <- [i + 1 .. lastI]
    ]
  where
    lastI = Vec.length v - 1

sinesRoughness :: AmpFreq -> AmpFreq -> Roughness
sinesRoughness (AmpFreq a1 f1) (AmpFreq a2 f2) =
    intensityDep * ampFlucDegDep * ampFlucRateDep
  where
    intensityDep = (a1 * a2) ** 0.1
    ampFlucDegDep = (2 * min a1 a2 / (a1 + a2)) ** 3.11
    ampFlucRateDep = exp (-3.5 * s * fDiff) - exp (-5.75 * s * fDiff)
    fDiff = abs (f1 - f2)
    s = 0.24 / (0.0207 * min f1 f2 + 18.96)

-- Let's say the lowest note must be C4, highest note B5 max.
-- In Haskore, HBP.intToFreq 21 = 440. 0 means C3.
allVoicings :: [Relative] -> [[Absolute]]
allVoicings [] = [[]]
allVoicings (0:xs) = map (12 :) $ allVoicings xs
allVoicings (x:xs) = concatMap
    (\ rest -> [x + 12 : rest, x + 24 : rest{-, x + 24 : rest-}]) $
    allVoicings xs

data ConsIntvl = ConsIntvl {unConsIntvl :: Int} deriving Eq

consOf :: ConsIntvl -> Int
consOf (ConsIntvl 1) = 38
consOf (ConsIntvl 2) = 36
consOf (ConsIntvl 3) = 15
consOf (ConsIntvl 4) = 10
consOf (ConsIntvl 5) = 6
consOf (ConsIntvl 6) = 20
consOf _ = error "consOf"

instance Ord ConsIntvl where
    compare = compare `on` consOf
 
instance Show ConsIntvl where
    show (ConsIntvl x) = show x   

main :: IO ()
--main = putStr . unlines $ chords 5 7
main = putStr . unlines $ chords 6 9

showNote :: Relative -> String
showNote x = showCl (intCl n) <> show (o + 3)
  where
    (o, n) = x `divMod` 12

pairIntvls :: ModeQ -> [Int]
pairIntvls (ModeQ v) = sortBy (flip compare)
    [diff (w Vec.! i) (w Vec.! j) | i <- [0 .. lastI], j <- [i + 1 .. lastI]]
  where
    w = Vec.scanl1 (+) v
    lastI = Vec.length v - 1
    diff x y = min ((x - y) `mod` 12) ((y - x) `mod` 12)

sixCyc :: [Int] -> [[Int]]
sixCyc = take 6 . map (take 6) . tails . cycle

avgAndMin :: [(Double, a)] -> (Double, a)
avgAndMin ((x0,a0):rest0) = go x0 1 x0 a0 rest0
  where
    go !xSum !len !_minX !minA [] = (xSum / len, minA)
    go !xSum !len !minX !minA ((x,a):rest) = if x < minX
      then go (xSum + x) (len + 1) x    a    rest
      else go (xSum + x) (len + 1) minX minA rest
avgAndMin _ = error "avgAndMin"

{-
chords :: Int -> Int -> [String]
chords noteNum padNum =
    map (\(Named n (chord, (r, v))) -> padr padNum ' ' n <> " " <>
        intercalate " " (map show . Vec.toList $ unMQ chord) <> "  " <>
        concatMap show (pairIntvls chord) <> "  " <>
        take 5 (show r) <> "  " <>
        intercalate " " (map showNote (sort v))) .
    sortBy (compare `on` fst . snd . unName) .
    map (onNamed (\(x, vs) ->
        ( x
        -- , minimum $ map (\v -> (myRough v, v)) vs
        , avgAndMin $ map (\v -> (myRough v, v)) vs
        )
        )) .
    --(Named "Maj" (replicate 6 0, [map (12 +) [0, 4, 7, 12, 16, 19]]) :) .
    map (onNamed (\x ->
        ( x
        , allVoicings . take 6 . scanl (+) 0 . Vec.toList $ unMQ x
        -- , concatMap (allVoicings . take 6 . scanl (+) 0) . sixCyc .
        --  Vec.toList $ unMQ x
        )
        )) $
    genNChords noteNum
    -}

chords :: Int -> Int -> [String]
chords noteNum padNum =
    map (\(Named n (chord, (intvls, diss))) -> padr padNum ' ' n <> " " <>
        intercalate " " (map show . Vec.toList $ unMQ chord) <> "  " <>
        concatMap show (sortBy (flip compare `on` intvlDissonance) intvls) <>
        "  " <> show diss) .
    sortBy (compare `on` snd . snd . unName) .
    map (onNamed (second (\intvls -> (intvls, intvlsDissonance intvls)))) .
    map (onNamed (\x -> (x, pairIntvls x))) $
    genNChords noteNum
