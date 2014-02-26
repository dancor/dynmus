{-# LANGUAGE BangPatterns #-}

import Basic
import Freq
import Portaudio
import SampTbl

main :: IO ()
main = do
    let sound = map (* 0.5) $
            zipWith (+)
            (realizeFreqs sinTbl
                (replicate (2 * sampleRate) 440 ++
                replicate (2 * sampleRate) 220)
            )
            (realizeFreqs sinTbl
                (replicate (2 * sampleRate) 880 ++
                replicate (2 * sampleRate) 440)
            )

            {-
        connectedNotes sinTbl 0 $
            map (\n -> (noteFreq n, sampleRate `div` 5)) $
            [Note 30 4 x | x <- [0..29]]
            [ (noteFreq $ Note 12 4 0, 2 * sampleRate)
            , (noteFreq $ Note 12 4 4, 2 * sampleRate)
            ]
            -}
    withPortaudio $ playSamples sound
