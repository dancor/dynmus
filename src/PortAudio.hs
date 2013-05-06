module PortAudio (withPortAudio, myAll) where

import Control.Applicative
import Control.Exception
import Foreign.C.Types

import qualified Base

type Error = String

maybeError :: CInt -> Maybe Error
maybeError i
    -- | i == Base.unPaErrorCode Base.paNoError = Nothing
    | i == 0                                 = Nothing
    | otherwise                              = Just "DEATH"  -- XX

initialize :: IO (Maybe Error)
initialize = maybeError <$> Base.c_Pa_Initialize

terminate :: IO (Maybe Error)
terminate = maybeError <$> Base.c_Pa_Terminate

withPortAudio :: IO (Either Error a) -> IO (Either Error a)
withPortAudio f =
    (initialize >>= maybe f (return . Left)) `finally` terminate

myAll :: IO Int
myAll = fromIntegral <$> Base.c_my_all
