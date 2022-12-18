module Common where
import System.Process
import Control.Concurrent
import Data.Maybe
import Data.Time.Clock.POSIX
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Exception.Base

class Duration a where
    duration :: a -> Int

loopWith' :: (Show result, Show ctx, Duration ctx) => ctx -> result -> (ctx -> result -> IO Bool) -> (result -> IO result) -> IO result
loopWith' ctx result basecase genResult = do
    basecaseRes <- basecase ctx result
    if basecaseRes then
        pure result
    else do
        threadDelay $ duration ctx
        result' <- genResult result
        loopWith' ctx result' basecase genResult

runBenchmarkFor :: ProcessHandle -> a -> (ProcessHandle -> a -> IO a) -> IO a
runBenchmarkFor ph base handler  =
    handler ph base 