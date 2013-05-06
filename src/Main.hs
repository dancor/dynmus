import Control.Monad.IO.Class
-- import Data.Either.Utils

import PortAudio

main :: IO ()
{-
main = fmap fromRight . withPortAudio $ do
    liftIO $ putStrLn "Lol."
    return $ Right ()
    -}
main = do
    ret <- myAll
    print ret
    return ()
