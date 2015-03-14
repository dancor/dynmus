module Main where

import Control.Wire
import Data.List
import Data.Monoid
--import qualified Data.Vector as Vec
import Prelude hiding ((.), id)
import System.Environment
import Data.Time

import PlaySig

type MyW a = WireP () a
type MyWD = MyW Double

buzz :: MyWD
buzz = noise (mkStdGen 0)

aFreq :: Double
aFreq = 440

sine :: Double -> MyWD
sine freq = (\t -> sin (2 * pi * freq * t)) <$> time

decPart :: Double -> Double
decPart x = snd (properFraction x :: (Int, Double))

sawtooth :: Double -> MyWD
sawtooth freq = (decPart . (* freq)) <$> time

avgPair :: MyWD -> MyWD -> MyWD
avgPair a b = (/ 2) <$> liftA2 (+) a b

note :: Double -> Double -> MyWD
note freq dur =
    --envOf (sawtooth freq `avgPair` buzz)
    myTone
  where
    riseDur = 0.1 * dur
    mainDur = 0.5 * dur
    fallDur = 0.4 * dur
    fromTo from to d = integral_ from . (pure $ (to - from) / dur) . for d
    riseEnv = fromTo 0 1 riseDur
    mainEnv = pure 1 . for mainDur
    fallEnv = fromTo 1 0 fallDur
    envOf = liftA2 (*) (riseEnv --> mainEnv --> fallEnv)

    myTone = envOf $ sine freq
    --myTone = envOf $ liftA2 (+) (sine (1.5 * freq)) (sine freq)
    {-myTone = 
        liftA2 (-) (envOf $ sawtooth freq) $
        liftA2 (*) (fromTo 1 0 0.1 <|> pure 0) buzz
        -}

metronomeNote1 :: Double -> MyWD
metronomeNote1 beatDur =
    note (2 * aFreq) noteDur <|> pure 0 . for beatDur
  where
    noteDur = 0.75 * beatDur

metronomeNoteN :: Double -> MyWD
metronomeNoteN beatDur =
    note aFreq noteDur <|> pure 0 . for beatDur
  where
    noteDur = 0.75 * beatDur

andThenN :: (Monad m, Monoid e) => Int -> Wire e m a b -> Wire e m a b
andThenN 0 _ = empty
andThenN n w
  | n >= 1 = foldl1' (-->) $ replicate n w
  | otherwise = error "andThenN: negative n"

capEnds :: MyWD -> MyWD
capEnds x = pure 0 . for 1 --> x --> pure 0 . for 1

-- Does nothing for one second initially.
-- Also does an extra bar before playing starts.
renderBeats :: Int -> [Double] -> MyWD
renderBeats beatsPerBar bpms =
    capEnds $
    foldl1' (-->) .
    zipWith (\n bpm -> n $ 60 / bpm)
    (cycle $ metronomeNote1 : replicate (beatsPerBar - 1) metronomeNoteN) $
    take beatsPerBar bpms ++ bpms

gradient :: Double -> Double -> Int -> [Double]
gradient pre post nBeats =
    map (\i -> pre + fromIntegral i * rate) [1 .. nBeats]
  where
    rate = (post - pre) / fromIntegral (nBeats + 1)

gradRep :: Double -> Double -> Int -> Int -> [Double]
gradRep t1 t2 gradN totN = gradient t1 t2 gradN ++ replicate (totN - gradN) t2

priestP1 :: [Double]
priestP1 = replicate (16 * 4) 76

priestP2 :: [Double]
priestP2 = replicate (16 * 4) 69

priest :: [Double]
priest = priestP1 ++ priestP2

t1Lento, t2Affrettando, t3Largamente :: Double
t1Lento       = 48
t2Affrettando = 75
t3Largamente  = 40 -- idk

t1BeatN, t2BeatN, t2Intro, t3BeatN :: Int
t1BeatN = 10 * 3
t2BeatN = 4 * 3
t2Intro = 1 * 3
t3BeatN = 3 * 3

agonyP1 :: [Double]
agonyP1 =
    replicate t1BeatN t1Lento ++
    gradRep t1Lento t2Affrettando t2Intro t2BeatN ++
    replicate t3BeatN t3Largamente

t4ATempo, t5Affrettando, t6Largamente :: Double
t4ATempo      = t1Lento
t5Affrettando = t2Affrettando
t6Largamente  = t3Largamente

t4BeatN, t5BeatN, t5Intro, t6BeatN :: Int
t4BeatN = 11 * 3
t5BeatN = 3 * 3
t5Intro = t2Intro
t6BeatN = 6 * 3

agonyP2 :: [Double]
agonyP2 =
    replicate t4BeatN t4ATempo ++
    gradRep t4ATempo t5Affrettando t5Intro t5BeatN ++
    replicate t6BeatN t6Largamente

t7PiuMosso, t8Allargando, t9PiuMosso, t10Allargando, t11Accel, t12PiuMosso,
    t13Largamente, t14Rit, t15Allargando, t16MoltoLento, t17Rit :: Double
t7PiuMosso    = 104
t8Allargando  = 80 -- idk
t9PiuMosso    = t7PiuMosso
t10Allargando = t8Allargando
t11Accel      = t7PiuMosso
t12PiuMosso   = t7PiuMosso
t13Largamente = 70 -- idk
t14Rit        = 60 -- idk
t15Allargando = 50 -- idk
t16MoltoLento = 45 -- idk
t17Rit        = 40 -- idk

t7BeatN, t8BeatN, t8Intro, t9BeatN, t10BeatN, t10Intro, t11BeatN, t11Intro,
    t12BeatN, t13BeatN, t14BeatN, t14Intro, t15BeatN, t16BeatN, t17BeatN,
    t17Intro :: Int
t7BeatN = 4 * 3
t8BeatN = 2 * 3
t8Intro = 1 * 3
t9BeatN = 4 * 3
t10BeatN = 2 * 3
t10Intro = t8Intro
t11BeatN = 4 * 3
t11Intro = t10BeatN
t12BeatN = 4 * 3
t13BeatN = 4 * 3 + 2
t14BeatN = 1 + 2 * 3 + 2
t14Intro = t14BeatN
t15BeatN = 1 + 2 * 3
t16BeatN = 3 * 3
t17BeatN = 4 * 3
t17Intro = t16BeatN

agonyP3And4 :: [Double]
agonyP3And4 = concat
    [ replicate t7BeatN t7PiuMosso
    , gradRep t7PiuMosso t8Allargando t8Intro t8BeatN
    , replicate t9BeatN t9PiuMosso
    , gradRep t9PiuMosso t10Allargando t10Intro t10BeatN
    , gradRep t10Allargando t11Accel t11Intro t11BeatN
    , replicate t12BeatN t12PiuMosso
    , replicate t13BeatN t13Largamente
    , gradRep t13Largamente t14Rit t14Intro t14BeatN
    , replicate t15BeatN t15Allargando
    , replicate t16BeatN t16MoltoLento
    , gradRep t16MoltoLento t17Rit t17Intro t17BeatN
    ]

agony :: [Double]
agony = agonyP1 ++ agonyP2 ++ agonyP3And4

testSig :: MyWD
testSig = 
    renderBeats 3 $
    replicate 3 104
    -- replicate t7BeatN t7PiuMosso

doMain :: [String] -> IO ()
doMain args = do
    let vol :: Double
        vol = 1.0
    case args of
      [] -> playSig vol testSig
      ["priest"] -> playSig vol $ renderBeats 4 priest
      ["priest1"] -> playSig vol $ renderBeats 4 priestP1
      ["priest2"] -> playSig vol $ renderBeats 4 priestP2
      ["agony"] -> playSig vol $ renderBeats 3 agony
      ["agony1"] -> playSig vol $ renderBeats 3 agonyP1
      ["agony2"] -> playSig vol $ renderBeats 3 agonyP2
      ["agony3"] -> playSig vol $ renderBeats 3 agonyP3And4
      ["agony12"] -> playSig vol . renderBeats 3 $ agonyP1 ++ agonyP2
      ["agony23"] -> playSig vol . renderBeats 3 $ agonyP2 ++ agonyP3And4
      ["bpm"] -> do
        _ <- getLine
        tStart0 <- getCurrentTime
        let bpmLoop tStart = do
                _ <- getLine
                tEnd <- getCurrentTime
                putStrLn $
                    show (round (60 / diffUTCTime tEnd tStart) :: Int) ++
                    " bpm"
                bpmLoop tEnd
        bpmLoop tStart0
      _ -> putStrLn "Usage: ./dynmus {yampa|table|sawtooth}"

main :: IO ()
main = getArgs >>= doMain
