import Data.Vector.Storable as DVS

import Portaudio

nextPhase :: Sig -> Sig
nextPhase phase =
    if phase' < 0.5
      then phase'
      else phase' - 2
  where
    phase' = phase + 0.01

midLols :: Sig -> Int -> IO ()
midLols _ 0 = return ()
midLols phase iterLeft = do
    let vec = DVS.iterateN framesPerBuffer nextPhase phase
    playVec vec
    midLols (nextPhase $ DVS.last vec) (iterLeft - 1)

main :: IO ()
main = withPortaudio $ midLols 0 (2 * sampleRate `div` framesPerBuffer)
