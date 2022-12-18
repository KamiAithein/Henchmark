{-# LANGUAGE InstanceSigs #-}
module Benchmark
    ( benchmark
    ) where

import System.Process
import Control.Concurrent
import Data.Maybe
import Data.Time.Clock.POSIX
import Control.Concurrent.STM
import Control.Concurrent.STM.TVar

import Control.Exception.Base

class Duration a where
    duration :: a -> Int

data TimingSpec = TimingSpec
    { tDuration  :: !Int
    , tEnd       :: !Bool
    , tRec       :: !Int -- recent
    }
    deriving (Show)

instance Duration TimingSpec where
    duration TimingSpec{tDuration=tDuration} = tDuration


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

runTimingBenchmark :: ProcessHandle -> IO TimingResult
runTimingBenchmark ph = do
    trStart    <- getPOSIXTime
    let timingResult =   TimingResult{trDuration=0, trStart=trStart, trLast=trStart, trPh=ph}  -- set this before use
    runBenchmarkFor ph timingResult loopOverTiming

benchmark :: String -> [String] -> IO TimingResult
benchmark pName pArgs = do
    (_, mhout, mherr, hp) <-
        createProcess (proc pName pArgs){ std_out = CreatePipe }
        -- It would be cool if I could create a process handle **And then run it**
    
    let result = runTimingBenchmark hp

    let hout =
            case mhout of
                Just hout -> hout
                Nothing   -> error "no output handle"
    let herr =
            case mherr of
                Just herr -> herr
                Nothing   -> error "no error handle"

    result


    -- where   timeThreadHandler :: TVar Bool -> Either SomeException except -> IO ()
    --         timeThreadHandler _ (Left err) = throw err
    --         timeThreadHandler tDone (Right tResult) = do
    --             let !_ = writeTVar tDone True
    --             pure ()

    --         waitOnThreadResult :: TVar Bool -> TVar TimingResult -> IO TimingResult
    --         waitOnThreadResult isReadyVar resultVar = do
    --             isReady <- readTVarIO isReadyVar
    --             if isReady then
    --                     readTVarIO resultVar
    --             else 
    --                 waitOnThreadResult isReadyVar resultVar


