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
    , parse_log
    ) where

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JCStress
import SBS.Common.Queries
import SBS.Common.Utils
import SBS.Common.Wilton

import Data
import Diff
import Parser

parseOutput :: Text -> IO JCStressResults
parseOutput path =
    withFileText path fun
    where
        fun tx = return (parseJCStressOutput tx path)

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

spawnJCStressAndWait :: JCStressConfig -> Text -> Text -> IO Text
spawnJCStressAndWait cf appd jdk = do
    if (enabled cf)
    then do
        createDirectory (unpack wd)
        code <- spawnProcess SpawnedProcessArgs
            { workDir = wd
            , executable = exec
            , execArgs = fromList
                [  ("-Xmx" <> (showText (xmxMemoryLimitMB cf)) <> "M")
                , "-jar", prependIfRelative appd (jcstressJarPath cf)
                , "-m", mode cf
                ]
            , outputFile = log
            , awaitExit = True
            }
        checkSpawnSuccess "jcstress" code log
    else
        copyFile (unpack mockLog) (unpack log)
    return log
    where
        wd = prependIfRelative appd (workDir (cf :: JCStressConfig))
        mockLog = prependIfRelative appd (mockOutput cf)
        log = wd <> "jcstress.log"
        exec = jdk <> "/bin/java"

finalizeDbEntry :: DBConnection -> Queries -> Int64 -> JCStressResults -> JCStressResultsDiff -> IO ()
finalizeDbEntry db qrs rid res diff = do
    curdate <- getCurrentTime
    dbExecute db (get qrs "updateRunFinish") (object
        [ "id" .= rid
        , "state" .= ("finished" :: Text)
        , "finishDate" .= formatISO8601 curdate
        , "passed" .= passedCount res
        , "passedDiff" .= passedDiff diff
        , "interesting" .= Vector.length (interesting res)
        , "interestingDiff" .= interestingDiff diff
        , "failed" .= Vector.length (failed res)
        , "failedDiff" .= failedDiff diff
        , "error" .= Vector.length (errored res)
        , "errorDiff" .= errorDiff diff
        ])
    return ()

run :: JCStressInput -> IO ()
run (JCStressInput ctx jdkDir cf) = do
    let tid = taskId ctx
    let db = dbConnection ctx
    let appd = appDir ctx
    let qdir = prependIfRelative appd (queriesDir ctx)
    qrs <- loadQueries (qdir <> "queries-jcstress.sql")
    rid <- dbWithSyncTransaction db (
        createDbEntry db qrs tid)
    log <- spawnJCStressAndWait cf appd jdkDir
    res <- parseOutput log
    bl <- parseOutput (prependIfRelative appd (baselineOutput cf))
    let diff = diffResults bl res
    dbWithSyncTransaction db ( do
        finalizeDbEntry db qrs rid res diff )
    return ()

parse_log :: Vector Text -> IO ()
parse_log arguments = do
    when (1 /= Vector.length arguments)
        ((error . unpack) "Path to 'jcstress.log' file must be specified as a first and only argument")
    res <- parseOutput (arguments ! 0)
    putStrLn (showText res)
    return ()
