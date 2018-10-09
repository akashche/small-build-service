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
import SBS.Common.JDKBuild
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
            cfres <- parseConfOutput (confOutPath (paths :: Paths))
            spawnMakeAndWait cf paths
            mres <- parseMakeOutput (makeOutPath (paths :: Paths))
            let _imageDir = pathConcat (confDirectory cfres) (imageDirRelative mres)
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
        db = dbConnection (ctx :: TaskContext)
        paths = resolvePaths ctx cf

results :: JDKBuildInput -> IO Text
results (JDKBuildInput ctx cf _eim) = do
    destd <- resolveDestDir paths based
    let jdkd = pathConcat destd "jdkbuild"
    createDirectory (unpack destd)
    createDirectory (unpack jdkd)
    copyFile (unpack (confOutPath paths)) (unpack (pathConcat jdkd (confOutputFile cf)))
    copyFile (unpack (makeOutPath paths)) (unpack (pathConcat jdkd (makeOutputFile cf)))
    return destd
    where
        paths = resolvePaths ctx cf
        based = destBaseDir ctx

-- test calls

runMock :: JDKBuildInput -> IO ()
runMock (JDKBuildInput ctx cf _) = do
    qrs <- loadQueries (queriesPath paths)
    jid <- dbWithSyncTransaction db (createJob db qrs (taskId ctx))
    dbWithSyncTransaction db (updateJobState db qrs jid StateRunning)
    repo <- readRepoUrl paths
    rev <- readRepoRevision paths
    dbWithSyncTransaction db (updateJobRepo db qrs jid repo rev)
    copyFile (unpack (pathConcat md "conf.log")) (unpack confOut)
    _cfres <- parseConfOutput confOut
    copyFile (unpack (pathConcat md "make.log")) (unpack makeOut)
    _mres <- parseMakeOutput makeOut
    dbWithSyncTransaction db (finalizeJob db qrs jid StateSuccess)
    return ()
    where
        db = dbConnection (ctx :: TaskContext)
        paths = resolvePaths ctx cf
        md = mockOutputDir (paths :: Paths)
        confOut = confOutPath (paths :: Paths)
        makeOut = makeOutPath (paths :: Paths)

pathsFromArgs :: Vector Text -> Paths
pathsFromArgs arguments =
    if (1 /= Vector.length arguments)
    then ((error . unpack) "Path to application directory must be specified as a first and only argument")
    else mockPaths (arguments ! 0)

spawnRepoUrl :: Vector Text -> IO ()
spawnRepoUrl arguments = do
    let paths = pathsFromArgs arguments
    dyloadModules ["wilton_process"]
    url <- readRepoUrl paths
    putStrLn url
    return ()

spawnRepoRevision :: Vector Text -> IO ()
spawnRepoRevision arguments = do
    let paths = pathsFromArgs arguments
    dyloadModules ["wilton_process"]
    rev <- readRepoRevision paths
    putStrLn rev
    return ()

spawnConf :: Vector Text -> IO ()
spawnConf arguments = do
    let paths = pathsFromArgs arguments
    dyloadModules ["wilton_process"]
    spawnConfigureAndWait mockConfig paths
    return ()

spawnMake :: Vector Text -> IO ()
spawnMake arguments = do
    let paths = pathsFromArgs arguments
    dyloadModules ["wilton_process"]
    spawnMakeAndWait mockConfig paths
    return ()

parseConf :: Vector Text -> IO ()
parseConf arguments = do
    when (1 /= Vector.length arguments)
        ((error . unpack) "Path to configure output must be specified as a first and only argument")
    cd <- parseConfOutput (arguments ! 0)
    putStrLn (showText cd)
    return ()

parseMake :: Vector Text -> IO ()
parseMake arguments = do
    when (1 /= Vector.length arguments)
        ((error . unpack) "Path to make output must be specified as a first and only argument")
    md <- parseMakeOutput (arguments ! 0)
    putStrLn (showText md)
    return ()

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    {           errRun <- registerWiltonCall "jdkbuild_run" run
    ; if isJust errRun then createWiltonError errRun

    ; else do { errResults <- registerWiltonCall "jdkbuild_results" results
    ; if isJust errResults then createWiltonError errResults

    ; else do { errMock <- registerWiltonCall "jdkbuild_run_mock" runMock
    ; if isJust errMock then createWiltonError errMock

    ; else do { errUrl <- registerWiltonCall "jdkbuild_spawn_repourl" spawnRepoUrl
    ; if isJust errUrl then createWiltonError errUrl

    ; else do { errRev <- registerWiltonCall "jdkbuild_spawn_reporev" spawnRepoRevision
    ; if isJust errRev then createWiltonError errRev

    ; else do { errConf <- registerWiltonCall "jdkbuild_spawn_conf" spawnConf
    ; if isJust errConf then createWiltonError errConf

    ; else do { errMake <- registerWiltonCall "jdkbuild_spawn_make" spawnMake
    ; if isJust errMake then createWiltonError errMake

    ; else do { errParseConf <- registerWiltonCall "jdkbuild_parse_conf" parseConf
    ; if isJust errParseConf then createWiltonError errParseConf

    ; else do { errParseMake <- registerWiltonCall "jdkbuild_parse_make" parseMake
    ; if isJust errParseMake then createWiltonError errParseMake

      else createWiltonError Nothing
    }}}}}}}}}

