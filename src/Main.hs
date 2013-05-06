module Main where

-- 

import Control.Monad
import qualified Data.Vector as V
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Sound.PortAudio
import Sound.PortAudio.Base

tableSize :: Int
tableSize = 200

sineTable :: V.Vector Float
sineTable =
    V.fromList $
    map (\i -> sin $ (fromIntegral i / fromIntegral tableSize) * pi * 2)
        [0 .. tableSize - 1]

poker :: (Storable a, Fractional a) => Ptr a -> Int -> Int -> IO Int
poker out phase i = do
    pokeElemOff out (2 * i)      (realToFrac $ sineTable V.! phase)
    pokeElemOff out (2 * i + 1)  (realToFrac $ sineTable V.! phase)
    return $ if phase >= tableSize then phase - tableSize else phase

customSettingsPlaySine :: IO (Either Error ())
customSettingsPlaySine = do
    let numChannels = 2
        framesPerBuffer = 1024
    defOutInfo <- getDefaultOutputInfo
    case defOutInfo of
        Left err -> return $ Left err
        Right (devIndex, devInfo) -> do
            let outInfo = Just $ StreamParameters devIndex 2
                    (defaultHighOutputLatency devInfo) 
            withStream Nothing outInfo 44100 (Just framesPerBuffer) [ClipOff]
                       Nothing Nothing $ \strm -> do
                Nothing <- startStream (strm :: Stream CFloat CFloat)
                allocaBytes (framesPerBuffer * numChannels) $ \out -> do
                    out' <- newForeignPtr_ out
                    let runFunc phase = do
                            phase' <- foldM (poker (out :: Ptr CFloat)) phase
                                [0 .. fromIntegral $ framesPerBuffer - 1]
                            Nothing <- writeStream strm
                                (fromIntegral framesPerBuffer) out'
                            return phase'
                    replicateM_ 100 $ runFunc 0
                Nothing <- stopStream strm
                return $ Right ()

main :: IO ()
main = do
    Right () <- withPortAudio customSettingsPlaySine
    return ()
