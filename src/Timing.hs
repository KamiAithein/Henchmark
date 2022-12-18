{-# LANGUAGE InstanceSigs #-}
module Timing where
import System.Process
import Control.Concurrent
import Data.Maybe
import Data.Time.Clock.POSIX
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Exception.Base
import Common

instance Duration TimingSpec where
    duration TimingSpec{tDuration=tDuration} = tDuration

data TimingSpec = TimingSpec
    { tDuration  :: !Int -- measurement period
    , tEnd       :: !Bool
    , tRec       :: !Int -- recent
    }
    deriving (Show)


data TimingResult = TimingResult
    { trDuration    :: !POSIXTime
    , trStart       :: !POSIXTime
    , trLast        :: !POSIXTime
    , trPh          :: !ProcessHandle
    }

instance Show TimingResult where
    show :: TimingResult -> String
    show TimingResult{trDuration=trDuration, trStart=trStart, trLast=trLast} =
        "TimingResult { trDuration = "
        ++ show trDuration
        ++ ", trStart = "
        ++ show trStart
        ++ ", trLast = "
        ++ show trLast
        ++ "}"

loopOverTiming :: ProcessHandle -> TimingResult -> IO TimingResult
loopOverTiming ph tr =
    let ts = TimingSpec{tDuration=1000,tEnd=False,tRec=0}
    in loopWith' ts tr  basecase genResult
    where   basecase :: TimingSpec -> TimingResult -> IO Bool
            basecase _ TimingResult{trDuration=trDuration, trPh=ph} = do
                    exitCode <- getProcessExitCode ph
                    pure $ isJust exitCode -- check if program errors out

            genResult :: TimingResult -> IO TimingResult
            genResult tr@TimingResult{trStart=trStart} = do
                time <- getPOSIXTime
                pure tr{trDuration=time-trStart, trLast=time}

runTimingBenchmark :: ProcessHandle -> IO TimingResult
runTimingBenchmark ph = do
    trStart    <- getPOSIXTime
    let timingResult =   TimingResult{trDuration=0, trStart=trStart, trLast=trStart, trPh=ph}  -- set this before use
    runBenchmarkFor ph timingResult loopOverTiming

