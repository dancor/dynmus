module PlaySig where

import Control.Concurrent
import qualified Control.Conditional as Cond
import Control.Monad
import Control.Wire hiding (when)
import Data.Audio
import Data.Int
import Data.IORef
import Foreign.Marshal
import Foreign.Storable
import qualified FRP.Yampa as Y
import Prelude hiding ((.), id)
import Sound.OpenAL

import Chunk (Chunk(..))

playSigYampa :: Y.SF () (Double, Y.Event ()) -> IO ()
playSigYampa sig = do
    let sampRate = 44100
        valsPerChunk = 4410
        formatSize = sizeOf (undefined :: Int16)
        chunkByteLen = valsPerChunk * formatSize
        bufNum = 2
    (dev, ctx, src, bufs) <- initOpenAL bufNum
    mbChunkMV <- newEmptyMVar
    iRef <- newIORef (0 :: Int, 0 :: Int)
    ptrs <- replicateM bufNum $ mallocBytes chunkByteLen
    _ <- forkIO $ process bufNum sampRate src bufs mbChunkMV

    -- Only this section differs between the Yampa and Netwire versions.
    let chunks = map (flip Chunk chunkByteLen) ptrs
        sense _ = return (1.0 / fromIntegral sampRate, Just ())
        actuate _ (s, e) =
            if Y.isEvent e
            then return True
            else do
                (chunkI, i) <- readIORef iRef
                pokeElemOff (ptrs !! chunkI) i $ fromSample s
                if i == valsPerChunk - 1
                    then do
                        putMVar mbChunkMV (Just $ chunks !! chunkI)
                        writeIORef iRef ((chunkI + 1) `mod` bufNum, 0)
                    else writeIORef iRef (chunkI, i + 1)
                return False
    Y.reactimate (return ()) sense actuate sig

    mapM_ free ptrs
    deInitOpenAL dev ctx src bufs

playSig :: WireP () Double -> IO ()
playSig sig = do
    (dev, ctx, src, bufs) <- initOpenAL bufNum
    mbChunkMV <- newEmptyMVar
    iRef <- newIORef (0 :: Int, 0 :: Int)
    ptrs <- replicateM bufNum $ mallocBytes chunkByteLen
    _ <- forkIO $ process bufNum sampRate src bufs mbChunkMV

    -- Only this section differs between the Yampa and Netwire versions.
    let chunks = map (flip Chunk chunkByteLen) ptrs
        myLoop myWire session = do
            (exOrSigVal, myWire', session') <- stepSessionP myWire session ()
            case exOrSigVal of
                Left _ex -> return ()
                Right sigVal -> do
                    (chunkI, i) <- readIORef iRef
                    pokeElemOff (ptrs !! chunkI) i $ fromSample sigVal
                    if i == valsPerChunk - 1
                      then do
                        putMVar mbChunkMV (Just $ chunks !! chunkI)
                        writeIORef iRef ((chunkI + 1) `mod` bufNum, 0)
                      else
                        writeIORef iRef (chunkI, i + 1)
                    myLoop myWire' session'
    myLoop sig clockSession

    mapM_ free ptrs
    deInitOpenAL dev ctx src bufs

maybeM :: (Monad m) => m (Maybe a) -> m b -> (a -> m b) -> m b
maybeM p n j = p >>= maybe n j

initOpenAL :: Int -> IO (Device, Context, Source, [Buffer])
initOpenAL bufNum = 
    maybeM (openDevice Nothing) (fail "opening OpenAL device") $ \ dev ->
    maybeM (createContext dev []) (fail "opening OpenAL context") $ \ ctx ->
        do
            currentContext $= Just ctx
            [src] <- genObjectNames 1
            bufs <- genObjectNames bufNum
            return (dev, ctx, src, bufs)

deInitOpenAL :: Device -> Context -> Source -> [Buffer] -> IO ()
deInitOpenAL dev ctx src bufs = do
    deleteObjectNames [src]
    deleteObjectNames bufs
    currentContext $= Nothing
    destroyContext ctx
    Cond.unlessM (closeDevice dev) $ fail "closing OpenAL device"

waitForBuffer :: Source -> IO ()
waitForBuffer src = do
    n <- get $ buffersProcessed src
    when (n == 0) $ waitForBuffer src

waitForSource :: Source -> IO ()
waitForSource src = do
    state <- get $ sourceState src
    when (state == Playing) $ threadDelay 10 >> waitForSource src

process :: Int -> Int -> Source -> [Buffer] -> MVar (Maybe Chunk) -> IO ()
process _bufNum sampRate src bufs mbChunkMV = do
    forM_ bufs $ \ buf -> maybeM
        (takeMVar mbChunkMV)
        (return ()) $ \ chunk -> do
            bufferData buf $= createBufferData sampRate chunk
            queueBuffers src [buf]
    play [src]
    sqncWhileTrue . cycle . flip map bufs $ \ buf -> maybeM
        (takeMVar mbChunkMV)
        (return False) $ \ chunk -> do
            waitForBuffer src
            unqueueBuffers src [buf]
            bufferData buf $= createBufferData sampRate chunk
            queueBuffers src [buf]
            return True

sqncWhileTrue :: (Monad m) => [m Bool] -> m ()
sqncWhileTrue [] = return ()
sqncWhileTrue (m:ms) = m >>= \ r -> when r (sqncWhileTrue ms)

createBufferData :: Int -> Chunk -> BufferData Int16
createBufferData sampRate chunk = BufferData
    (MemoryRegion
        (Chunk.dataPtr chunk)
        (fromIntegral $ Chunk.byteLen chunk))
    Mono16
    (fromIntegral sampRate)
