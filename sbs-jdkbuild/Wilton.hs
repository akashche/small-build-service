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

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JDKBuild
import SBS.Common.Parsec
import SBS.Common.Queries
import SBS.Common.Wilton
import SBS.Common.Utils

import Data
import DB
import Lib
import Parser
import Spawn

run :: JDKBuildInput -> IO ()
run (JDKBuildInput ctx cf _eim) = do
    createDirectory (unpack (workDir (paths :: Paths)))
    createDirectory (unpack (buildDir (paths :: Paths)))
    qrs <- loadQueries (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    catch
        (do
            dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
            repo <- readRepoUrl paths
            rev <- readRepoRevision paths
            dbWithSyncTransaction db (updateJobRepo db qrs jid repo rev)
            spawnConfigureAndWait cf paths
            cfres <- parseFile configureDetailsParser (confOutPath (paths :: Paths))
            spawnMakeAndWait cf paths
            mres <- parseFile makeDetailsParser (makeOutPath (paths :: Paths))
            let _imageDir = (confDirectory cfres) <> (imageDirRelative mres)
            -- TODO
--             when (eim /= imageDir) ((error . unpack)(
--                    "Invalid image directory,"
--                 <> " expected: [" <> eim <> "]"
--                 <> " actual: [" <> imageDir <> "]"))
            dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess))
        (\(e :: SomeException) -> do
            dbWithSyncTransaction db (finalizeJob db qrs jid StateError)
            (error . unpack) (showText e))
    return ()
    where
        db = dbConnection ctx
        paths = resolvePaths ctx cf

runMock :: JDKBuildInput -> IO ()
runMock (JDKBuildInput ctx cf _) = do
    qrs <- loadQueries (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
    repo <- readRepoUrl paths
    rev <- readRepoRevision paths
    dbWithSyncTransaction db (updateJobRepo db qrs jid repo rev)
    copyFile (unpack (md <> "conf.log")) (unpack confOut)
    _cfres <- parseFile configureDetailsParser confOut
    copyFile (unpack (md <> "make.log")) (unpack makeOut)
    _mres <- parseFile makeDetailsParser makeOut
    dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess)
    return ()
    where
        db = dbConnection ctx
        paths = resolvePaths ctx cf
        md = mockOutputDir (paths :: Paths)
        confOut = confOutPath (paths :: Paths)
        makeOut = makeOutPath (paths :: Paths)

spawnConf :: Vector Text -> IO ()
spawnConf _ = do
    dyloadModules ["wilton_process"]
    -- todo
    return ()

spawnMake :: Vector Text -> IO ()
spawnMake _ = do
    dyloadModules ["wilton_process"]
    -- todo
    return ()

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    {           errRun <- registerWiltonCall "jdkbuild_run" run
    ; if isJust errRun then createWiltonError errRun

    ; else do { errRunMock <- registerWiltonCall "jdkbuild_run_mock" runMock
    ; if isJust errRunMock then createWiltonError errRunMock

    ; else do { errConf <- registerWiltonCall "jdkbuild_spawn_conf" spawnConf
    ; if isJust errConf then createWiltonError errConf

    ; else do { errMake <- registerWiltonCall "jdkbuild_spawn_make" spawnMake
    ; if isJust errMake then createWiltonError errMake

      else createWiltonError Nothing
    }}}}

