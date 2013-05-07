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
import Prelude hiding ((.), id)
import Sound.OpenAL

import Chunk (Chunk(..))
import ForkWait

playSig :: WireP () Double -> IO ()
playSig sig = do
    let sampRate = 44100
        -- Higher values still cause total running time to get progressively
        -- shorter than it should be.  Not sure why though, since we are
        -- pushing the last block (full block even, right?) when sig ends.
        --valsPerChunk = 8820
        --valsPerChunk = 4410
        valsPerChunk = 2205
        -- 1102 is too low and it hangs.  Not sure why though.
        -- Bug in Haskell code here, or something with OpenAL?
        --valsPerChunk = 1102
        formatSize = sizeOf (undefined :: Int16)
        chunkByteLen = valsPerChunk * formatSize
        bufNum = 2
    (dev, ctx, src, bufs) <- initOpenAL bufNum
    mbChunkMV <- newEmptyMVar
    iRef <- newIORef (0 :: Int, 0 :: Int)
    ptrs <- replicateM bufNum $ mallocBytes chunkByteLen
    processW <-
        snd <$> forkIOWaitable (process bufNum sampRate src bufs mbChunkMV)
    let chunks = map (flip Chunk chunkByteLen) ptrs
        myLoop myWire session = do
            (exOrSigVal, myWire', session') <- stepSessionP myWire session ()
            case exOrSigVal of
              Left _ex -> do
                (chunkI, _i) <- readIORef iRef
                -- Though the latter part of this chunk will be old?
                putMVar mbChunkMV (Just $ chunks !! chunkI)
                putMVar mbChunkMV Nothing
              Right sigVal -> do
                (chunkI, i) <- readIORef iRef
                pokeElemOff (ptrs !! chunkI) i $ fromSample sigVal
                if i == valsPerChunk - 1
                  then do
                    putMVar mbChunkMV (Just $ chunks !! chunkI)
                    writeIORef iRef ((chunkI + 1) `mod` bufNum, 0)
                    --putStrLn "."
                  else
                    writeIORef iRef (chunkI, i + 1)
                myLoop myWire' session'
    myLoop sig $ counterSession (1 / fromIntegral sampRate)
    forkedIOWait processW
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
sqncWhileTrue (m:ms) = Cond.whenM m $ sqncWhileTrue ms

createBufferData :: Int -> Chunk -> BufferData Int16
createBufferData sampRate chunk = BufferData
    (MemoryRegion
        (Chunk.dataPtr chunk)
        (fromIntegral $ Chunk.byteLen chunk))
    Mono16
    (fromIntegral sampRate)
