{-# LANGUAGE BangPatterns #-}

module Basic where

import Data.Fixed
import Data.List
import qualified Data.Vector.Unboxed as DVU

import Freq
import SampTbl

realizeFreqs :: SampTbl -> [Freq] -> [Sample]
realizeFreqs !tbl =
    snd . mapAccumL doAdvance 0 . map (* tblRateFactor)
  where
    tblSizeF = fromIntegral $ DVU.length tbl
    tblRateFactor = tblSizeF / sampRateF

    doAdvance :: Float -> Float -> (Float, Sample)
    doAdvance tblPos tblAdvance = (newTblPos, tbl DVU.! floor tblPos)
      where
        newTblPos = (tblPos + tblAdvance) `mod'` tblSizeF
