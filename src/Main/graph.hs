{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import Data.Function
import Data.GraphViz
import Data.GraphViz.Attributes.Complete
import Data.GraphViz.Printing
import Data.Graph.Inductive
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import Data.Monoid
import Data.Ord
import qualified Data.Text.Lazy as DTL
import qualified Data.Text.Lazy.IO as DTLI
import qualified Data.Vector as Vec
import Safe

import Chord
import Hexachord
import Named

theChords :: [Named ModeQ]
theChords =
    [ Named "nu"    cNu
    , Named "mano"  cMano
    , Named "namni" cNamni
    , Named "nemne" cNemne
    , Named "niman" cNiman
    , Named "nom"   cNom
    ]

main :: IO ()
main = do
    mapM_ (\(a, b) -> putStrLn $ padr 9 ' ' a ++ " " ++ b) $
        sortBy (comparing (head . fst) `mappend` comparing snd)
        [ ( show (deg myGraph n `div` 2) ++ fromJustNote "b" (lab myGraph n)
          , intercalate "," (map show . sort .
              map ((`div` 2) . deg myGraph) . nub . sort $
              neighbors myGraph n) ++ " " ++
            intercalate "," (sort .
              map (\n2 -> show (deg myGraph n2 `div` 2) ++
                  fromJust (lab myGraph n2)) .
              nub . sort $
              neighbors myGraph n)
          )
        | (_, n) <- zip theChords [0..]
        ]
    --prettyPrint myGraph
    DTLI.writeFile "hex.dot" $ renderDot $ toDot $
        graphToDot myParams myGraph
        {-
        graphElemsToDot nonClusteredParams
        (zip [0..] (map snd hexachords))
        (concatMap edgesForMq mqAndNums)
        -}

mqAndNums :: [(ModeQ, Int)]
mqAndNums = zip (map unName theChords) [0..]

mqToNum :: Map.Map (Vec.Vector Int) Int
mqToNum = Map.fromList $ map (first $ minimum . vRotPoss . unMQ) mqAndNums

mqMods :: ModeQ -> [Vec.Vector Int]
mqMods (ModeQ xs) =
    if Vec.last xs == 1 then normPoss else movePoss : normPoss
  where
    normPoss = map Vec.fromList $ cqcMods' $ Vec.toList xs
    movePoss = Vec.update xs $ Vec.fromList
        [(0, Vec.head xs + 1), (Vec.length xs - 1, Vec.last xs - 1)]

cqcMods' :: [Int] -> [[Int]]
cqcMods' [] = []
cqcMods' [x] = []
cqcMods' (x1:x2:xs) = if x1 == 1 then normPoss else movePoss : normPoss
  where
    normPoss = map (x1:) (cqcMods' (x2:xs))
    movePoss = x1 - 1 : x2 + 1 : xs

edgesForMq :: (ModeQ, Int) -> [UEdge]
edgesForMq (mq, i) = map (\j -> (i, j, ())) .
    catMaybes .
    map (flip Map.lookup mqToNum) .
    nub . sort . map (minimum . vRotPoss) $
    mqMods mq

flipEdges :: [UEdge] -> [UEdge]
flipEdges =
    nub . sort . concatMap (\(a, b, ()) -> [(a, b, ()), (b, a, ())])

myGraph :: Gr String ()
myGraph = mkGraph
    (zip [0..] (map getName theChords))
    (flipEdges $ concatMap edgesForMq mqAndNums)

--myParams = blankParams
myParams = nonClusteredParams
    { isDirected = False
    , globalAttributes = [GraphAttrs [Start (StartSeed 1)]]
    , clusterBy = \(n, l) -> C (deg myGraph n `div` 2) $ N (n, l)
    , clusterID = Num . Int
    , fmtCluster = clFmt
    , fmtNode = \(_, x) -> [Label (StrLabel $ DTL.pack x)]
    , fmtEdge = const []
    , isDotCluster = const True
    }
  where
    clFmt m = [GraphAttrs [toLabel $ "Degree " ++ show m]]

padr :: Int -> a -> [a] -> [a]
padr n c cs = cs ++ replicate (n - length cs) c

{-
evenOdd :: (Graph gr, Ord el) => gr Int el -> DotGraph Node
evenOdd = setDirectedness graphToDot params
  where
    params = blankParams { globalAttributes = []
                         , clusterBy        = clustBy
                         , clusterID        = Num . Int
                         , fmtCluster       = clFmt
                         , fmtNode          = const []
                         , fmtEdge          = const []
                         }
    clustBy (n,l) = C (n `mod` 2) $ N (n,l)
    clFmt m = [GraphAttrs [toLabel $ "n == " ++ show m ++ " (mod 2)"]]
-}
