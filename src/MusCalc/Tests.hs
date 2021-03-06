module MusCalc.Tests where

import Control.Applicative
import qualified Data.Vector as Vec
--import qualified Test.QuickCheck as QC
--import qualified Test.QuickCheck.Monadic as QC
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Chord
import Hexachord
import MusCalc
import Named

testCV1 :: Assertion
testCV1 = calcVoicings 1 (-24, 24) (3, 14) (Vec.singleton 1) @?= [[14]]

testCV2 :: Assertion
testCV2 =
    any (null . calcVoicings 0 (-24, 36) (3, 14) . mqOrd . unName) hexachords
    @?= False

chromaticRunSimple :: Int -> Bool
chromaticRunSimple n =
    [[1..m]] == calcVoicings 0 (-24, 24) (1, 1) (Vec.fromList [1..m])
  where
    m = n `mod` 12

tests :: TestTree
tests = testGroup "MusCalc"
    [ testCase "testCV1" testCV1
    , testCase "testCV2" testCV2
    , testProperty "chromaticRunSimple" chromaticRunSimple
    ]
