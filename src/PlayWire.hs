module PlayWire (playWire, wireToList) where

import Control.Wire
import Data.List.Split
import Prelude hiding ((.), id)

import Portaudio
import SampTbl

wireToList :: Session Identity s -> WireP s () () b -> [b]
wireToList session wire =
  let Identity (ds, session') = stepSession session
      Identity (bOrE, wire') = stepWire wire ds (Right ())
  in case bOrE of
    Right b -> b : wireToList session' wire'
    _ -> []

playWire :: Wire (Timed Float ()) () Identity () Sample -> IO ()
playWire =
    mapM_ playSamples .
    chunksOf framesPerBuffer .
    wireToList (countSession (1 / sampleRateF) <*> pure ())
