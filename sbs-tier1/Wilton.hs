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
import SBS.Common.Queries
import SBS.Common.Tier1
import SBS.Common.Utils
import SBS.Common.Wilton

import DB
import Lib
import Parser
import Spawn

run :: Tier1Input -> IO ()
run (Tier1Input ctx cf) = do
    createDirectory (unpack (workDir (paths :: Paths)))
    qrs <- loadQueries (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    catch
        (do
            dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
            spawnTestsAndWait paths (fromList [target cf])
            res <- parseTier1File (outputPath (paths :: Paths))
            saveResults db qrs jid res
            extractSummary (outputPath (paths :: Paths)) (summaryPath paths)
            dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess))
        (\(e :: SomeException) -> do
            dbWithSyncTransaction db (finalizeJob db qrs jid StateError)
            (error . unpack) (showText e))
    return ()
    where
        db = dbConnection ctx
        paths = resolvePaths ctx cf

runMock :: Tier1Input -> IO ()
runMock (Tier1Input ctx cf) = do
    createDirectory (unpack (workDir (paths :: Paths)))
    qrs <- loadQueries (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    catch
        (do
            dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
            copyFile (unpack (mockOutputPath (paths :: Paths))) (unpack (outputPath (paths :: Paths)))
            res <- parseTier1File (outputPath (paths :: Paths))
            saveResults db qrs jid res
            extractSummary (outputPath (paths :: Paths)) (summaryPath paths)
            dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess))
        (\(e :: SomeException) -> do
            dbWithSyncTransaction db (finalizeJob db qrs jid StateError)
            (error . unpack) (showText e))
    return ()
    where
        db = dbConnection ctx
        paths = resolvePaths ctx cf

spawn :: Vector Text -> IO ()
spawn _ = do
    dyloadModules ["wilton_process"]
    spawnTestsAndWait paths (fromList ["run-test-tier1"])
    where
        paths = Paths
            { workDir = ""
            , buildDir = ""
            , execPath = "/usr/bin/make"
            , outputPath = "tier1.log"
            , mockOutputPath = ""
            , summaryPath = "tier1-summary.log"
            , queriesPath = ""
            }

parse :: Vector Text -> IO ()
parse arguments = do
    when (1 /= Vector.length arguments)
        ((error . unpack) "Path to tier1 tests output must be provided as a first and only argument")
    parsed <- parseTier1File (arguments ! 0)
    putStrLn (showText parsed)
    return ()

-- tier1_test_shortlog:

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    { errRun <- registerWiltonCall "tier1_run" run
    ; if isJust errRun then createWiltonError errRun

    ; else do { errRunMock <- registerWiltonCall "tier1_run_mock" runMock
    ; if isJust errRun then createWiltonError errRunMock

    ; else do { errSpawn <- registerWiltonCall "tier1_spawn" spawn
    ; if isJust errSpawn then createWiltonError errSpawn

    ; else do { errParse <- registerWiltonCall "tier1_parse" parse
    ; if isJust errParse then createWiltonError errParse

      else createWiltonError Nothing
    }}}}

