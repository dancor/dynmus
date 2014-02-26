{-# LANGUAGE BangPatterns #-}

module SampTbl where

import qualified Data.Vector.Unboxed as DVU

type SampTbl = DVU.Vector Sample

type Sample = Float

sinTblSize :: Int
sinTblSize = 10000

sinTbl :: SampTbl
sinTbl = genSinTbl sinTblSize

genSinTbl :: Int -> SampTbl
genSinTbl !tblSize =
    DVU.generate tblSize (sin . (xChange *) . fromIntegral)
  where
    xChange :: Float
    xChange = 2 * pi / fromIntegral tblSize
