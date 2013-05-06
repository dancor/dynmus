{-# LANGUAGE ForeignFunctionInterface #-}

module Base where

import Foreign.C.Types

#include "portaudio.h"
#include "my_f.h"

foreign import ccall "my_f.h my_f"
    c_my_f :: CInt -> CInt

foreign import ccall "my_f.h my_all"
    c_my_all :: IO CInt

foreign import ccall "portaudio.h Pa_Initialize"
    c_Pa_Initialize :: IO CInt

foreign import ccall "portaudio.h Pa_GetDefaultOutputDevice"
    c_Pa_GetDefaultOutputDevice :: IO CInt

foreign import ccall "portaudio.h Pa_Terminate"
    c_Pa_Terminate :: IO CInt
