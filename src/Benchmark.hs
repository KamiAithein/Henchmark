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

import Timing

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


