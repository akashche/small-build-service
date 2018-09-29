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

module Lib
    ( run
    ) where

import Prelude ()
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JDKBuild
import SBS.Common.Parsec
import SBS.Common.Queries
import SBS.Common.Utils
import SBS.Common.Wilton

import Data
import Parser

createDbEntry :: DBConnection -> Queries -> Int64 -> IO Int64
createDbEntry db qrs tid = do
    dbExecute db (get qrs "updateRunsId") Empty
    (IncrementedSeq idx) <- dbQueryObject db (get qrs "selectRunsId") Empty
    curdate <- getCurrentTime
    dbExecute db (get qrs "insertRun") (object
        [ "id" .= idx
        , "startDate" .= formatISO8601 curdate
        , "state" .= ("running" :: Text)
        , "taskId" .= tid
        ])
    return idx

finalizeDbEntry :: DBConnection -> Queries -> Int64 -> Text -> Text -> IO ()
finalizeDbEntry db qrs rid repo rev = do
    curdate <- getCurrentTime
    dbExecute db (get qrs "updateRunFinish") (object
        [ "id" .= rid
        , "finishDate" .= formatISO8601 curdate
        , "state" .= ("finished" :: Text)
        , "repository" .= repo
        , "revision" .= rev
        ])

readRepoUrl :: JDKBuildConfig -> Text -> Text -> IO Text
readRepoUrl cf appd wd = do
    let log = wd <> "repourl.log"
    code <- spawnProcess SpawnedProcessArgs
        { workDir = prependIfRelative appd (sourceDir cf)
        , executable = prependIfRelative appd (hgPath cf)
        , execArgs = fromList [ "paths", "default"]
        , outputFile = log
        , awaitExit = True
        }
    checkSpawnSuccess "jdkbuild_repourl" code log
    url <- readFile (unpack log)
    return (Text.strip url)

readRepoRevision :: JDKBuildConfig -> Text -> Text -> IO Text
readRepoRevision cf appd wd = do
    let log = wd <> "revision.log"
    code <- spawnProcess SpawnedProcessArgs
        { workDir = prependIfRelative appd (sourceDir cf)
        , executable = prependIfRelative appd (hgPath cf)
        , execArgs = fromList [ "id", "-i"]
        , outputFile = log
        , awaitExit = True
        }
    checkSpawnSuccess "jdkbuild_revision" code log
    rev <- readFile (unpack log)
    return (Text.strip rev)

spawnConfigureAndWait :: JDKBuildConfig -> Text -> Text -> Text -> IO Text
spawnConfigureAndWait cf appd wd bd = do
    if enabled cf
    then do
        code <- spawnProcess SpawnedProcessArgs
            { workDir = bd
            , executable = prependIfRelative appd (bashPath cf)
            , execArgs = fromList [ "-c", argsstr ]
            , outputFile = log
            , awaitExit = True
            }
        checkSpawnSuccess "jdkbuild_conf" code log
    else do
        let mockLog = prependIfRelative appd ((mockOutputDir cf) <> "conf.log")
        copyFile (unpack mockLog) (unpack log)
    return log
    where
        log = wd <> "conf.log"
        sd = prependIfRelative appd (sourceDir cf)
        args = Vector.concat [fromList
            [ sd <> "configure"
            , "--with-boot-jdk=" <> (prependIfRelative appd (bootJdkDir cf))
            , "--with-jtreg=" <> (prependIfRelative appd (jtregDir cf))
            , "--with-log=" <> (logLevel cf)
            ], additionalConfigureArguments cf]
        folder ac el = ac <> " " <> el
        -- https://help.appveyor.com/discussions/problems/4150-0-bad-file-descriptor-error-running-configure
        argsstr = "cat /dev/null | bash " <> (Vector.foldl1' folder args)

spawnMakeAndWait :: JDKBuildConfig -> Text -> Text -> Text -> IO Text
spawnMakeAndWait cf appd wd bd = do
    let log = wd <> "make.log"
    if enabled cf
    then do
        code <- spawnProcess SpawnedProcessArgs
            { workDir = bd
            , executable = prependIfRelative appd (makePath cf)
            , execArgs = fromList [target cf]
            , outputFile = log
            , awaitExit = True
            }
        checkSpawnSuccess "jdkbuild_make" code log
    else do
        let mockLog = prependIfRelative appd ((mockOutputDir cf) <> "make.log")
        copyFile (unpack mockLog) (unpack log)
    return log

run :: JDKBuildInput -> IO JDKBuildOutput
run (JDKBuildInput ctx cf) = do
    let tid = taskId ctx
    let db = dbConnection ctx
    let appd = appDir ctx
    let wd = prependIfRelative appd (workDir (cf :: JDKBuildConfig))
    let bd = wd <> "build"
    let qdir = prependIfRelative appd (queriesDir ctx)
    qrs <- loadQueries (qdir <> "queries-jdkbuild.sql")
    rid <- dbWithSyncTransaction db (
        createDbEntry db qrs tid)
    when (enabled cf) ( do
        createDirectory (unpack wd)
        createDirectory (unpack bd))
    repo <- readRepoUrl cf appd wd
    rev <- readRepoRevision cf appd wd
    cflog <- spawnConfigureAndWait cf appd wd bd
    cfres <- parseFile configureDetailsParser cflog
    mlog <- spawnMakeAndWait cf appd wd bd
    mres <- parseFile makeDetailsParser mlog
    dbWithSyncTransaction db (
        finalizeDbEntry db qrs rid repo rev )
    let imageDir = (confDirectory cfres) <> (imageDirRelative mres)
    return (JDKBuildOutput imageDir)
