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
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JCStress
import SBS.Common.Queries
import SBS.Common.Utils
import SBS.Common.Wilton

import Data
import DB
import Lib
import Parser
import Spawn

run :: JCStressInput -> IO ()
run (JCStressInput ctx cf) = do
    createDirectory (unpack (workDir (paths :: Paths)))
    qrs <- loadQueries (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    catch
        (do
            dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
            spawnJCStressAndWait cf paths
            res <- parseResults (outputPath (paths :: Paths))
            saveResults db qrs jid res
            writeFile (unpack (summaryPath paths)) (formatSummary res)
            dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess (totalFailOrError res)))
        (\(e :: SomeException) -> do
            dbWithSyncTransaction db (finalizeJob db qrs jid StateError 0)
            (error . unpack) (showText e))
    return ()
    where
        db = dbConnection (ctx :: TaskContext)
        paths = resolvePaths ctx cf

diff :: DiffRequest -> IO Text
diff req = do
    qrs <- loadQueries (resolveQueriesPath req "jcstress")
    res1 <- loadResults db qrs (taskId1 req)
    res2 <- loadResults db qrs (taskId2 req)
    when (1 /= Vector.length res1 || 1 /= Vector.length res2)
        ((error . unpack) "Invalid task")
    let rd = diffResultsCount (res1 ! 0) (res2 ! 0)
    let tx = formatResultsDiff rd
    return tx
    where
        db = dbConnection (req :: DiffRequest)

-- test calls

runMock :: JCStressInput -> IO ()
runMock (JCStressInput ctx cf) = do
    qrs <- loadQueries (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
    copyFile (unpack (mockOutputPath (paths :: Paths))) (unpack (outputPath (paths :: Paths)))
    res <- parseResults (outputPath (paths :: Paths))
    saveResults db qrs jid res
    writeFile (unpack (summaryPath paths)) (formatSummary res)
    dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess (totalFailOrError res))
    where
        db = dbConnection (ctx :: TaskContext)
        paths = resolvePaths ctx cf

spawn :: Vector Text -> IO ()
spawn _ = do
    dyloadModules ["wilton_process"]
    spawnJCStressAndWait  mockConfig mockPaths

parse :: Vector Text -> IO ()
parse arguments = do
    when (1 /= Vector.length arguments)
        ((error . unpack) "Path to jcstress output must be specified as a first and only argument")
    parsed <- parseResults (arguments ! 0)
    putStrLn (showText parsed)
    return ()

summary :: Vector Text -> IO ()
summary arguments = do
    when (1 /= Vector.length arguments)
        ((error . unpack) "Path to jcstress output must be specified as a first and only argument")
    res <- parseResults (arguments ! 0)
    let sm = formatSummary res
    putStrLn sm
    return ()

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    {           errRun <- registerWiltonCall "jcstress_run" run
    ; if isJust errRun then createWiltonError errRun

    ; else do { errDiff <- registerWiltonCall "jcstress_diff" diff
    ; if isJust errDiff then createWiltonError errDiff

    ; else do { errRunMock <- registerWiltonCall "jcstress_run_mock" runMock
    ; if isJust errRunMock then createWiltonError errRunMock

    ; else do { errSpawn <- registerWiltonCall "jcstress_spawn" spawn
    ; if isJust errSpawn then createWiltonError errSpawn

    ; else do { errParse <- registerWiltonCall "jcstress_parse" parse
    ; if isJust errParse then createWiltonError errParse

    ; else do { errSummary <- registerWiltonCall "jcstress_summary" summary
    ; if isJust errSummary then createWiltonError errSummary

      else createWiltonError Nothing
    }}}}}}

