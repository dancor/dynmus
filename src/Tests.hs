import Test.Tasty
import qualified MusCalc.Tests

main :: IO ()
main = defaultMain $ testGroup "Tests"
    [ MusCalc.Tests.tests
    ]
