module ForkWait where

import Control.Concurrent

forkIOWaitable :: IO a -> IO (ThreadId, MVar a)
forkIOWaitable m = do
    w <- newEmptyMVar
    tId <- forkIO (m >>= putMVar w)
    return (tId, w)

forkedIOWait :: MVar a -> IO a
forkedIOWait = takeMVar
