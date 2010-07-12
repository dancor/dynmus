{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}

module AlsaMidi where

import Control.Applicative
import Foreign.C
import Foreign.Ptr
import Foreign.Marshal
 
data SndSeqT

data SndSeqAddrT

type SndSeqTP = Ptr (Ptr (Ptr SndSeqT))

type SndSeqAddrTP = Ptr (Ptr SndSeqAddrT)

data S7r = S7r SndSeqTP SndSeqAddrTP (Ptr CInt)

data Tick = Tick {fromTick :: Int}
  deriving (Eq, Ord, Show)

tickSum (Tick a) (Tick b) = Tick $ a + b

data Chan = Chan {fromChan :: Int}
  deriving (Eq, Ord, Show)

data Instr = Instr {fromInstr :: Int}
  deriving (Eq, Ord, Show)

data Note = Note {fromNote :: Int}
  deriving (Eq, Ord, Show)

data Vel = Vel {fromVel :: Int}
  deriving (Eq, Ord, Show)

-- not sure why, but when i make everything unsafe then set_instrument seems
-- to stop working iff there is not a debug line printed in it..

foreign import ccall safe "midi_initialize" c_midi_initialize :: 
  SndSeqTP -> SndSeqAddrTP -> Ptr CInt -> CString -> CString -> IO ()
midiInitialize :: String -> String -> IO S7r
midiInitialize clientName addressStr = do
  pSeq <- malloc
  pDestPort <- malloc
  pQueue <- malloc
  withCString clientName $ \ cClientName -> 
    withCString addressStr $ \ cAddressStr -> do
      c_midi_initialize pSeq pDestPort pQueue cClientName cAddressStr
      return $ S7r pSeq pDestPort pQueue

fI = fromIntegral

foreign import ccall safe "set_instrument" c_set_instrument :: 
  SndSeqTP -> SndSeqAddrTP -> Ptr CInt -> CInt -> CInt -> CInt -> IO ()
setInstrument :: S7r -> Tick -> Chan -> Instr -> IO ()
setInstrument (S7r pSeq pDestPort pQueue) (Tick t) (Chan c) (Instr i) =
  c_set_instrument pSeq pDestPort pQueue (fI t) (fI c) (fI i)

foreign import ccall safe "note_on" c_note_on :: 
  SndSeqTP -> SndSeqAddrTP -> Ptr CInt -> CInt -> CInt -> CInt -> CInt -> IO ()
noteOn :: S7r -> Tick -> Chan -> Note -> Vel -> IO ()
noteOn (S7r pSeq pDestPort pQueue) (Tick t) (Chan c) (Note n) (Vel v) =
  c_note_on pSeq pDestPort pQueue (fI t) (fI c) (fI n) (fI v)

foreign import ccall safe "note_off" c_note_off :: 
  SndSeqTP -> SndSeqAddrTP -> Ptr CInt -> CInt -> CInt -> CInt -> IO ()
noteOff :: S7r -> Tick -> Chan -> Note -> IO ()
noteOff (S7r pSeq pDestPort pQueue) (Tick t) (Chan c) (Note n) =
  c_note_off pSeq pDestPort pQueue (fI t) (fI c) (fI n)

foreign import ccall safe "midi_finalize" c_midi_finalize :: 
  SndSeqTP -> SndSeqAddrTP -> IO ()
midiFinalize :: S7r -> IO ()
midiFinalize (S7r pSeq pDestPort pQueue) = do
  c_midi_finalize pSeq pDestPort
  free pQueue
  free pDestPort
  free pSeq

