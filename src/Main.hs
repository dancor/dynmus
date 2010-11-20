{-# LANGUAGE Arrows #-}

import Data.Audio
import FRP.Yampa

import PlaySignal

data Note = C | Cs | D | Ds | E | F | Fs | G | Gs | A | As | B
  deriving Enum
-- piano is (Tone A 0) to (Tone C 8), 8 octaves + change
-- human hearing is about (Tone C 0) to (Tone C 10), 10 octaves
data Tone = Tone Note Int
type Freq = Double
type Ampl = Double

{-
intToTone i = i % 12

toneToInt
-}

toRates _ [] = []
toRates x0 ((t1, x1):txs) = (x1 - x0) / t1 : toRates x1 txs

--amplEnvelope :: Ampl -> [(Time, Ampl)] -> SF a (Ampl, Event ())
amplEnvelope :: Ampl -> [(Time, Ampl)] -> (SF a Ampl, Time)
amplEnvelope a0 tas = 
  (afterEach trs >>> hold r0 >>> integral >>> arr (+ a0), sum ts)
  where
  trs = zip ts rs
  ts = map fst tas
  r0:rs = toRates a0 tas

main :: IO ()
main = playSignal lol

bellEnvelope :: (SF a Ampl, Time)
bellEnvelope = amplEnvelope 0 [(0.1, 1), (10, 1), (10.25, 0)]

{-
noteToFreq :: Tone Note, Int) -> Freq
-}

sinWave :: Freq -> SF a Sample
sinWave f = time >>> arr (\ t -> 0.999 * sin (2 * pi * f * t))

fI = fromIntegral

toneFreq (Tone n o) = 55 * 2 ** (fI o + fI (fromEnum n - 21) / 12)

bellTone :: Tone -> (SF a Sample, Time)
bellTone t = first (\ e -> (e &&& sinWave (toneFreq t)) >>> arr (uncurry (*)))
  bellEnvelope

note1 :: SF a (Sample, Event ())
--note1 = arr id &&& (afterEach [(1, 440)] >>> hold 330) >>> freqToSin
note1 = (\ (s, t) -> s &&& after t ()) . bellTone $ Tone G 4
--arr id &&& (afterEach [(1, 440)] >>> hold 330) >>> freqToSin

{-
note2 :: SF Time Sample
note2 = arr id &&& constant 440 >>> freqToSin 

each2Sec :: SF a b -> SF a b -> SF a (b, Event ())
each2Sec f g =
  (f &&& after 2 ()) `switch` const g &&& after 4 ()
-}
--envelope :: /n

lol :: SF () (Sample, Event ())
lol = note1
