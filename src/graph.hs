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

import Chord

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

theChords = namedHexachords -- namedHexachords

hcIntL :: [(ChordQual, Int)]
hcIntL = zip (map fst theChords) [0..]

hcIntM :: Map.Map ChordQual Int
hcIntM = Map.fromList $ map (first normalizeCq) hcIntL

cqcMods :: [Int] -> [[Int]]
cqcMods xs = if last xs == 1 then normPoss else movePoss : normPoss
  where
    normPoss = cqcMods' xs
    movePoss = head xs + 1 : tail (init xs) ++ [last xs - 1]

cqcMods' :: [Int] -> [[Int]]
cqcMods' [] = []
cqcMods' [x] = []
cqcMods' (x1:x2:xs) = if x1 == 1 then normPoss else movePoss : normPoss
  where
    normPoss = map (x1:) (cqcMods' (x2:xs))
    movePoss = x1 - 1 : x2 + 1 : xs

fromJustE :: String -> Maybe a -> a
fromJustE _ (Just a) = a
fromJustE s Nothing = error s

edgesForHc :: (ChordQual, Int) -> [UEdge]
edgesForHc (hc, i) = map (\j -> (i, j, ())) $
    map (fromJustE "a" . flip Map.lookup hcIntM) .
    nub . sort . map normalizeCqc .
    cqcMods $ cqComplete hc

flipEdges :: [UEdge] -> [UEdge]
flipEdges =
    nub . sort . concatMap (\(a, b, ()) -> [(a, b, ()), (b, a, ())])

myGraph :: Gr String ()
myGraph = mkGraph
    (zip [0..] (map snd theChords))
    (flipEdges $ concatMap edgesForHc hcIntL)

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

main :: IO ()
main = do
    mapM_ (\(a, b) -> putStrLn $ padr 9 ' ' a ++ " " ++ b) $
        sortBy (comparing (head . fst) `mappend` comparing snd)
        [ ( show (deg myGraph n `div` 2) ++ fromJustE "b" (lab myGraph n)
          , intercalate "," (map show . sort .
              map ((`div` 2) . deg myGraph) . nub . sort $
              neighbors myGraph n) ++ " " ++
            intercalate "," (sort .
              map (\n2 -> show (deg myGraph n2 `div` 2) ++
                  fromJust (lab myGraph n2)) .
              nub . sort $
              neighbors myGraph n)
          )
        | n <- [0 .. length theChords - 1]
        ]
    --prettyPrint myGraph
    DTLI.writeFile "hex.dot" $ renderDot $ toDot $
        graphToDot myParams myGraph
        {-
        graphElemsToDot nonClusteredParams
        (zip [0..] (map snd hexachords))
        (concatMap edgesForHc hcIntL)
        -}
