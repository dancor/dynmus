{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Data.Function
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Set as Set
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.IO as DTLI
import qualified Data.Vector as Vec
import Safe
import System.Environment

import Chord
import Cl
import Hexachord
import Named

data TranStats = TranStats
    { tSharedNoteNum :: Int
    , tDist :: Int
    }

theNmqs :: [Named ModeQ]
theNmqs =
    [ Named "mano"  cMano
    , Named "namni" cNamni
    , Named "nemne" cNemne
    , Named "niman" cNiman
    , Named "nom"   cNom
    -- , Named "nu"    cNu
    ]

statsForTran :: ClSet -> ClSet -> TranStats
statsForTran a b = TranStats
    (Set.size $ Set.intersection a b)
    (clSetTranDist a b)

ctt :: String -> [(Named ClSet, TranStats)]
ctt arg = map (\x -> (x, statsForTran start $ unName x))
    [nmqAtCl nmq (Cl cl) | nmq <- theNmqs, cl <- [0 .. 11]]
  where
    startNmq = head $ filter ((== arg) . getName) theNmqs
    start = unName $ nmqAtCl startNmq (Cl 0)

main :: IO ()
main = do
    [arg] <- getArgs
    mapM_ putStrLn $ map
        (\(Named n _, TranStats s d) ->
            n <> "\t" <> show s <> "\t" <> show d
        ) $
        sortBy (
            (flip compare `on` tSharedNoteNum . snd) <>
            (compare `on` tDist . snd)) $
        ctt arg
