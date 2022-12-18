module Main (main) where

import System.Environment (getArgs)
import Benchmark

main :: IO ()
main = do
    (pName:pArgs) <- getArgs
    bResult <- benchmark pName pArgs
    print bResult
