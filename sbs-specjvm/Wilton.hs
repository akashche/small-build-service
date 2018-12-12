--
-- Copyright 2018, akashche at redhat.com
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Wilton ( ) where

import Prelude ()
import VtUtils.Prelude
import qualified System.Directory as Directory

import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.SpecJVM
import SBS.Common.Wilton

import Data
import DB
import Lib
import Parser
import Spawn

run :: SpecJVMInput -> IO ()
run (SpecJVMInput ctx cf) = do
    Directory.createDirectory (unpack (workDir (paths :: Paths)))
    qrs <- queriesLoad (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    resEither <- try
        (do
            dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
            spawnSpecAndWait cf paths
            res <- parseResults out
            saveResults db qrs jid res
            summ <- parseSummary out
            writeFile (unpack (summaryPath paths)) summ
            dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess (totalTimeSeconds res)))
    case resEither of
        Left (e :: SomeException) -> do
            dbWithSyncTransaction db (finalizeJob db qrs jid StateError 0)
            (error . unpack) (textShow e)
        Right _ -> return ()
    where
        db = dbConnection (ctx :: TaskContext)
        paths = resolvePaths ctx cf
        out = outputPath (paths :: Paths)

diff :: DiffRequest -> IO Text
diff req = do
    qrs <- queriesLoad (resolveQueriesPath req "specjvm")
    res1 <- loadResults db qrs (taskId1 req)
    res2 <- loadResults db qrs (taskId2 req)
    let rd = diffResults (Results 1 res1) (Results 1 res2)
    let tx = formatResultsDiff rd
    return tx
    where
        db = dbConnection (req :: DiffRequest)

results :: SpecJVMInput -> IO ()
results (SpecJVMInput ctx cf) = do
    let destd = pathConcat based "specjvm"
    Directory.createDirectory (unpack destd)
    Directory.copyFile (unpack outsrc) (unpack (outdest destd))
    Directory.copyFile (unpack sumsrc) (unpack (sumdest destd))
    return ()
    where
        paths = resolvePaths ctx cf
        based = destDir ctx
        outsrc = outputPath paths
        outdest dir = pathConcat dir (outputFile (cf :: SpecJVMConfig))
        sumsrc = summaryPath paths
        sumdest dir = pathConcat dir (summaryFile cf)

-- test calls

runMock :: SpecJVMInput -> IO ()
runMock (SpecJVMInput ctx cf) = do
    qrs <- queriesLoad (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
    Directory.copyFile (unpack (mockOutputPath (paths :: Paths))) (unpack (outputPath (paths :: Paths)))
    res <- parseResults (outputPath (paths :: Paths))
    saveResults db qrs jid res
    summ <- parseSummary out
    writeFile (unpack (summaryPath paths)) summ
    dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess (totalTimeSeconds res))
    where
        db = dbConnection (ctx :: TaskContext)
        paths = resolvePaths ctx cf
        out = outputPath (paths :: Paths)

spawn :: Vector Text -> IO ()
spawn _ = do
    dyloadModules ["wilton_process"]
    spawnSpecAndWait  mockConfig mockPaths

parse :: Vector Text -> IO ()
parse arguments = do
    when (1 /= length arguments)
        ((error . unpack) "Path to specjvm output must be specified as a first and only argument")
    parsed <- parseResults (arguments ! 0)
    putStrLn (textShow parsed)
    return ()

summary :: Vector Text -> IO ()
summary arguments = do
    when (1 /= length arguments)
        ((error . unpack) "Path to specjvm output must be specified as a first and only argument")
    sm <- parseSummary (arguments ! 0)
    putStrLn sm
    return ()

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    {           errRun <- registerWiltonCall "specjvm_run" run
    ; if isJust errRun then createWiltonError errRun

    ; else do { errResults <- registerWiltonCall "specjvm_results" results
    ; if isJust errResults then createWiltonError errResults

    ; else do { errDiff <- registerWiltonCall "specjvm_diff" diff
    ; if isJust errDiff then createWiltonError errDiff

    ; else do { errRunMock <- registerWiltonCall "specjvm_run_mock" runMock
    ; if isJust errRunMock then createWiltonError errRunMock

    ; else do { errSpawn <- registerWiltonCall "specjvm_spawn" spawn
    ; if isJust errSpawn then createWiltonError errSpawn

    ; else do { errParse <- registerWiltonCall "specjvm_parse" parse
    ; if isJust errParse then createWiltonError errParse

    ; else do { errSummary <- registerWiltonCall "specjvm_summary" summary
    ; if isJust errSummary then createWiltonError errSummary

      else createWiltonError Nothing
    }}}}}}}

