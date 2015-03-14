import Criterion.Main
import qualified Data.Vector.Unboxed as DVU

import SampTbl

sinTblSmall :: SampTbl
sinTblSmall = genSinTbl 10

sinTblBig :: SampTbl
sinTblBig = genSinTbl 1000000

small :: Int -> Sample
small x = sinTblSmall DVU.! x

big :: Int -> Sample
big x = sinTblBig DVU.! x

small2 :: Int -> Sample
small2 x = t DVU.! x
  where t = sinTblSmall

big2 :: Int -> Sample
big2 x = t DVU.! x
  where t = sinTblBig

main :: IO ()
main = defaultMain
    [ bench "small" $ nf small 1
    , bench "big" $ nf big 1
    , bench "small2" $ nf small2 1
    , bench "big2" $ nf big2 1
    ]
