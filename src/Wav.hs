module Wave where

import Data.WAVE

saveWave :: FilePath -> [Sample] -> IO ()
saveWave f sound = do
    let chanNum = 1
    putWAVEFile f .
        WAVE (WAVEHeader chanNum framePerSec 32 (Just framePerSec)) .
        map (:[]) $ map doubleToSample mySound
