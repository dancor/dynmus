module Main where

import System.IO
import System.Process

main :: IO ()
main = do
  print "hi"
  (pIn, pOut, pErr, pId) <- runInteractiveProcess "csound"
    ["-d", "-odac", "-L", "stdin", "hack.csd"]
    Nothing Nothing
  hClose pIn
  waitForProcess pId
  print "done"

