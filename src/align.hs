import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Monoid
import System.Environment

padr :: Int -> Char -> BS.ByteString -> BS.ByteString
padr n c s = s <> BSC.replicate (n - BS.length s) c

colMerge :: [(BS.ByteString, BS.ByteString)] -> [BS.ByteString]
colMerge rows = map (\(c1, c2) -> padr col1Width ' ' c1 <> c2) rows
  where
    col1Width = maximum $ map (BS.length . fst) rows

main :: IO ()
main = do
    [alignStr] <- getArgs
    BS.interact $ BSC.unlines . colMerge .
        map (BS.breakSubstring (BSC.pack alignStr)) . BSC.lines
