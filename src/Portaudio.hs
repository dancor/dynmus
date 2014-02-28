{-# LANGUAGE ForeignFunctionInterface #-}

module Portaudio where

import Control.Exception
import Control.Monad
import qualified Data.Vector.Storable as DVS
import Foreign
import Foreign.C.Types

type Sample = Float

foreign import ccall unsafe "lol.h start_lol"
    c_start_lol :: IO CInt

foreign import ccall unsafe "lol.h mid_lol"
    c_mid_lol :: Ptr CFloat -> CInt -> IO CInt

foreign import ccall unsafe "lol.h end_lol"
    c_end_lol :: IO CInt

framesPerBuffer :: Int
framesPerBuffer = 32

numChans :: Int
numChans = 1

dieOnErr :: IO CInt -> IO ()
dieOnErr f = do
  ret <- f
  when (ret /= 0) $ error "Aborting due to PortAudio error."

withPortaudio :: IO a -> IO a
withPortaudio f = bracket_ (dieOnErr c_start_lol) (dieOnErr c_end_lol) f

playSamples :: [Sample] -> IO ()
playSamples [] =  return ()
playSamples samps = do
    let (buffer, rest) = splitAt framesPerBuffer samps
    playVec (DVS.map realToFrac $ DVS.fromList buffer)
    playSamples rest

playVec :: DVS.Vector CFloat -> IO ()
playVec vec = DVS.unsafeWith vec $ \ptr ->
    dieOnErr (c_mid_lol ptr . fromIntegral $ DVS.length vec)
