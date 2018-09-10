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

spawnProcessAndWait :: SpecJVMConfig -> Text -> IO Text
spawnProcessAndWait cf jdk = do
    if (enabled cf)
    then do
        code <- wiltoncall "process_spawn" (object
            [ "executable" .= exec
            , "args" .= fromList
                [  ("-Xmx" <> (showText (xmxMemoryLimitMB cf)) <> "M")
                , "-jar", specjvmJarPath cf
                , "-t", (showText (threadsCount cf))
                , "-e", exreg (excludedBenchmarks cf)
                ]
            , "outputFile" .= log
            , "awaitExit" .= True
            ]) :: IO Int
        when (0 /= code) (throwSpawnFail code)
    else
        copyFile (unpack (mockOutput cf)) logStr
    return log
    where
        log = "specjvm.log" :: Text
        logStr = (unpack log)
        exec = jdk <> "/bin/java"
        sepNonEmpty st = if Text.length st > 0 then st <> "|" else st
        folder ac el = (sepNonEmpty ac) <> (Text.replace "." "\\." el)
        exreg vec = Vector.foldl' folder "" vec
        throwSpawnFail code = do
            outex <- doesFileExist logStr
            out <- if outex then readFile logStr else return ""
            errorText ("Error running SPECjvm,"
                <> " code: [" <> (showText code) <>"]"
                <> " output: [" <> (Text.strip out) <> "]")

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

run :: SpecJVMInput -> IO ()
run input = do
    let cf = specjvmConfig input
    let db = dbConnection input
    let qrs = queries input
    rid <- dbWithSyncTransaction db (
        createDbEntry db qrs (taskId input))
    log <- spawnProcessAndWait cf (jdkImageDir input)
    res <- parseOutput log
    bl <- parseOutput (baselineOutput cf)
    let diff = diffResults bl res
    dbWithSyncTransaction db ( do
        saveResults db qrs rid res
        saveDiff db qrs rid diff
        finalizeDbEntry db qrs rid (totalTimeSeconds res) (relativeTotalTime diff) )
    return ()
