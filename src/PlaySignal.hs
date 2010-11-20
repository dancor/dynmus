module PlaySignal (playSignal) where

import Control.Concurrent
import Control.Monad
import Control.Monad.Tools
import Data.Audio
import Data.Int
import Data.IORef
import FRP.Yampa
import Sound.OpenAL
import Foreign

import Chunk (Chunk(..))
import qualified Chunk

playSignal :: SF () (Sample, Event ()) -> IO ()
playSignal sig = do
  let
    sampleRate = 44100
    valsPerChunk = 4410
    formatSize = sizeOf (undefined :: Int16)
    chunkByteLen = valsPerChunk * formatSize
    bufNum = 2
  (dev, ctx, src, bufs) <- initOpenAL bufNum
  mbChunkMV <- newEmptyMVar
  iRef <- newIORef (0 :: Int, 0 :: Int)
  ptrs <- replicateM bufNum $ mallocBytes chunkByteLen
  let
    chunks = map (flip Chunk chunkByteLen) ptrs
  _ <- forkIO $ process bufNum sampleRate src bufs mbChunkMV
  let
    chunks = map (flip Chunk chunkByteLen) ptrs
    sense _ = return (1.0 / fromIntegral sampleRate, Just ())
    actuate _ (s, e) = if isEvent e
      then return True
      else do
        (chunkI, i) <- readIORef iRef
        pokeElemOff (ptrs !! chunkI) i $ fromSample s
        if i == valsPerChunk - 1
          then do
            putMVar mbChunkMV (Just $ chunks !! chunkI)
            writeIORef iRef ((chunkI + 1) `mod` bufNum, 0)
          else do
            writeIORef iRef (chunkI, i + 1)
        return False
  reactimate (return ()) sense actuate sig
  deInitOpenAL dev ctx src bufs
  mapM_ free ptrs

maybeM :: (Monad m) => m (Maybe a) -> m b -> (a -> m b) -> m b
maybeM p n j = p >>= maybe n j

initOpenAL :: Int -> IO (Device, Context, Source, [Buffer])
initOpenAL bufNum = 
  maybeM (openDevice Nothing) (fail "opening OpenAL device") $ \ dev ->
  maybeM (createContext dev []) (fail "opening OpenAL context") $ \ ctx -> do
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
  unlessM (closeDevice dev) $ fail "closing OpenAL device"

waitForBuffer :: Source -> IO ()
waitForBuffer src = do
  n <- get $ buffersProcessed src
  when (n == 0) $ waitForBuffer src

waitForSource :: Source -> IO ()
waitForSource src = do
  state <- get $ sourceState src
  when (state == Playing) $ threadDelay 10 >> waitForSource src

process :: Int -> Int -> Source -> [Buffer] -> MVar (Maybe Chunk) -> IO ()
process bufNum sampleRate src bufs mbChunkMV = do
  forM_ bufs $ \ buf -> maybeM
    (takeMVar mbChunkMV)
    (return ()) $ \ chunk ->
    do
      bufferData buf $= createBufferData sampleRate chunk
      queueBuffers src [buf]
  play [src]
  sqncWhileTrue . cycle . flip map bufs $ \ buf -> maybeM
    (takeMVar mbChunkMV)
    (return False) $ \ chunk ->
    do
      waitForBuffer src
      unqueueBuffers src [buf]
      bufferData (buf) $= createBufferData sampleRate chunk
      queueBuffers src [buf]
      return True

sqncWhileTrue :: (Monad m) => [m Bool] -> m ()
sqncWhileTrue [] = return ()
sqncWhileTrue (m:ms) = m >>= \ r -> when r (sqncWhileTrue ms)

createBufferData :: Int -> Chunk -> BufferData Int16
createBufferData sampleRate chunk = BufferData
  (MemoryRegion (Chunk.dataPtr chunk) (fromIntegral $ Chunk.byteLen chunk))
  Mono16
  (fromIntegral sampleRate)
