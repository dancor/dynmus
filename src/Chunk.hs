module Chunk where

import Data.Int
import Foreign.Ptr

data Chunk = Chunk {
  dataPtr :: Ptr Int16,
  byteLen :: Int
  }

