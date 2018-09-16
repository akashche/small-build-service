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
import SBS.Common.Queries
import SBS.Common.SpecJVM
import SBS.Common.Utils
import SBS.Common.Wilton

import Data
import Diff
import Parser

parseOutput :: Text -> IO SpecJVMResults
parseOutput path =
    withFileText path fun
    where
        fun tx = return (parseSpecJVMOutput tx path)

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

spawnSpecJVMAndWait :: SpecJVMConfig -> Text -> Text -> IO Text
spawnSpecJVMAndWait cf appd jdk = do
    if (enabled cf)
    then do
        createDirectory (unpack wd)
        code <- spawnProcess SpawnedProcessArgs
            { workDir = wd
            , executable = exec
            , execArgs = fromList
                [  ("-Xmx" <> (showText (xmxMemoryLimitMB cf)) <> "M")
                , "-jar", specjvmJarPath cf
                , "-t", (showText (threadsCount cf))
                , "-e", exreg (excludedBenchmarks cf)
                ]
            , outputFile = log
            , awaitExit = True
            }
        checkSpawnSuccess "specjvm" code log
    else
        copyFile (unpack mockLog) (unpack log)
    return log
    where
        wd = prependIfRelative appd (workDir (cf :: SpecJVMConfig))
        log = wd <> "specjvm.log"
        mockLog = prependIfRelative appd (mockOutput cf)
        exec = jdk <> "/bin/java"
        sepNonEmpty st = if Text.length st > 0 then st <> "|" else st
        folder ac el = (sepNonEmpty ac) <> (Text.replace "." "\\." el)
        exreg vec = Vector.foldl' folder "" vec

finalizeDbEntry :: DBConnection -> Queries -> Int64 -> Int -> Int -> IO ()
finalizeDbEntry db qrs rid totalTime relativeTime = do
    curdate <- getCurrentTime
    dbExecute db (get qrs "updateRunFinish") (object
        [ "id" .= rid
        , "state" .= ("finished" :: Text)
        , "finishDate" .= formatISO8601 curdate
        , "totalTimeSeconds" .= totalTime
        , "relativeTotalTime" .= relativeTime
        ])
    return ()

saveResults :: DBConnection -> Queries -> Int64 -> SpecJVMResults -> IO ()
saveResults db qrs rid res =
    Vector.mapM_ fun (benchmarks (res :: SpecJVMResults))
    where
        fun bench = do
            dbExecute db (get qrs "updateResultsId") Empty
            (IncrementedSeq idx) <- dbQueryObject db (get qrs "selectResultsId") Empty
            dbExecute db (get qrs "insertResult") (object
                [ "id" .= idx
                , "name" .= (name (bench :: BenchResult))
                , "mode" .= showText (mode bench)
                , "counts" .= (count bench)
                , "score" .= (score bench)
                , "error" .= (error bench)
                , "units" .= showText (units bench)
                , "runId" .= rid
                ])

saveDiff :: DBConnection -> Queries -> Int64 -> SpecJVMResultsDiff -> IO ()
saveDiff db qrs rid diff =
    Vector.mapM_ fun (benchmarks (diff :: SpecJVMResultsDiff))
    where
        fun bench = do
            dbExecute db (get qrs "updateDiffsId") Empty
            (IncrementedSeq idx) <- dbQueryObject db (get qrs "selectDiffsId") Empty
            dbExecute db (get qrs "insertDiff") (object
                [ "id" .= idx
                , "name" .= (name (bench :: BenchDiff))
                , "relativeScore" .= (relativeScore bench)
                , "runId" .= rid
                ])

copyNcNote :: SpecJVMConfig -> Text -> IO ()
copyNcNote cf appd = copyFile (unpack from) (unpack to)
    where
        from = prependIfRelative appd (ncNotePath cf)
        wd = prependIfRelative appd (workDir (cf :: SpecJVMConfig))
        to = wd <> "nc_note.txt"

run :: SpecJVMInput -> IO ()
run (SpecJVMInput ctx jdkDir cf) = do
    let tid = taskId ctx
    let db = dbConnection ctx
    let appd = appDir ctx
    let qdir = prependIfRelative appd (queriesDir ctx)
    qrs <- loadQueries (qdir <> "queries-specjvm.sql")
    rid <- dbWithSyncTransaction db (
        createDbEntry db qrs tid)
    log <- spawnSpecJVMAndWait cf appd jdkDir
    res <- parseOutput log
    copyNcNote cf (appDir ctx)
    bl <- parseOutput (prependIfRelative appd (baselineOutput cf))
    let diff = diffResults bl res
    dbWithSyncTransaction db ( do
        saveResults db qrs rid res
        saveDiff db qrs rid diff
        finalizeDbEntry db qrs rid (totalTimeSeconds res) (relativeTotalTime diff) )
    return ()
