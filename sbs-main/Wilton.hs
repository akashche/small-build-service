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
import qualified Data.Vector as Vector
import qualified System.Directory as Directory

import SBS.Common.Data
import SBS.Common.JCStress
import SBS.Common.JDKBuild
import SBS.Common.SpecJVM
import SBS.Common.Tier1
import SBS.Common.Wilton

import Data
import DB
import Lib

run :: Vector Text -> IO ()
run arguments = do
    (cf, db, qrs) <- initApp arguments
    ctx <- dbWithTransaction db (initTask cf db qrs)
    resEither <- try
        (do
            dbWithSyncTransaction db (updateTaskState db qrs (taskId ctx) StateRunning)
            when (enabled (jdkbuild cf :: JDKBuildConfig))
                (wiltoncall "jdkbuild_run" (JDKBuildInput ctx (jdkbuild cf) eim))
            when (enabled (tier1 cf :: Tier1Config))
                (wiltoncall "tier1_run" (Tier1Input ctx (tier1 cf)))
            when (enabled (jcstress cf :: JCStressConfig))
                (wiltoncall "jcstress_run" (JCStressInput ctx (jcstress cf)))
            when (enabled (specjvm cf :: SpecJVMConfig))
                (wiltoncall "specjvm_run" (SpecJVMInput ctx (specjvm cf)))
            dbWithTransaction db (finalizeTask db qrs (taskId ctx) StateSuccess))
    case resEither of
        Left (e :: SomeException) -> do
            dbWithSyncTransaction db (finalizeTask db qrs (taskId ctx) StateError)
            (error . unpack) (textShow e)
        Right _ -> return ()
    putStrLn "Run finished"
    return ()
    where
        eim = "" -- TODO

diff :: Vector Text -> IO ()
diff arguments = do
    when (3 /= Vector.length arguments)
        (error "Invalid arguments specified, expected: [path/to/config.json, taskId1, taskId2]")
    (cf, db, _) <- initApp arguments
    let tid1 = (read . unpack) (arguments ! 1) :: Int64
    let tid2 = (read . unpack) (arguments ! 2) :: Int64
    let appd = appDir (sbs (cf :: Config) :: SBSConfig)
    let qdir = queriesDir (database (sbs (cf :: Config) :: SBSConfig) :: DBConfig)
    let req = DiffRequest tid1 tid2 db appd qdir
    -- tier1
    tier1Diff <- wiltoncall "tier1_diff" req :: IO Text
    putStrLn "tier1:"
    putStrLn tier1Diff
    -- jcstress
    jcstressDiff <- wiltoncall "jcstress_diff" req :: IO Text
    putStrLn "jcstress:"
    putStrLn jcstressDiff
    -- specjvm
    specjvmDiff <- wiltoncall "specjvm_diff" req :: IO Text
    putStrLn "specjvm:"
    putStrLn specjvmDiff
    return ()

results :: Vector Text -> IO ()
results arguments = do
    (ctxjdk, cf, based) <- initResults arguments
    direx <- Directory.doesDirectoryExist (unpack based)
    unless (direx) (Directory.createDirectory (unpack based))
    destd <- wiltoncall "jdkbuild_results" (JDKBuildInput ctxjdk (jdkbuild cf) "") :: IO Text
    let ctx = ctxjdk {destDir = destd}
    wiltoncall "tier1_results" (Tier1Input ctx (tier1 cf)) :: IO ()
    wiltoncall "jcstress_results" (JCStressInput ctx (jcstress cf)) :: IO ()
    wiltoncall "specjvm_results" (SpecJVMInput ctx (specjvm cf)) :: IO ()
    return ()

-- test calls

runMock :: Vector Text -> IO ()
runMock arguments = do
    (cf, db, qrs) <- initApp arguments
    ctx <- dbWithTransaction db (initTask cf db qrs)
    wiltoncall "jdkbuild_run_mock" (JDKBuildInput ctx (jdkbuild cf) "") :: IO ()
    wiltoncall "tier1_run_mock" (Tier1Input ctx (tier1 cf)) :: IO ()
    wiltoncall "jcstress_run_mock" (JCStressInput ctx (jcstress cf)) :: IO ()
    wiltoncall "specjvm_run_mock" (SpecJVMInput ctx (specjvm cf)) :: IO ()
    dbWithTransaction db (finalizeTask db qrs (taskId ctx) StateSuccess)
    putStrLn "MOCK Run finished"
    return ()


foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    {           errRun <- registerWiltonCall "run" run
    ; if isJust errRun then createWiltonError errRun

    ; else do { errResults <- registerWiltonCall "results" results
    ; if isJust errResults then createWiltonError errResults

    ; else do { errDiff <- registerWiltonCall "diff" diff
    ; if isJust errDiff then createWiltonError errDiff

    ; else do { errRunMock <- registerWiltonCall "run_mock" runMock
    ; if isJust errRunMock then createWiltonError errRunMock

      else createWiltonError Nothing
    }}}}
