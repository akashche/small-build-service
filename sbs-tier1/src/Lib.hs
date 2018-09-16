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

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.Tier1
import SBS.Common.Utils
import SBS.Common.Wilton

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

finalizeDbEntry :: DBConnection -> Queries -> Int64 -> IO ()
finalizeDbEntry db qrs rid = do
    curdate <- getCurrentTime
    dbExecute db (get qrs "updateRunFinish") (object
        [ "id" .= rid
        , "finishDate" .= formatISO8601 curdate
        , "state" .= ("finished" :: Text)
        ])

spawnMakeAndWait :: Tier1Config -> Text -> IO ()
spawnMakeAndWait cf appd = do
    let wd = prependIfRelative appd (workDir (cf :: Tier1Config))
    let bd = wd <> "build"
    let log = wd <> "tier1.log"
    if enabled cf
    then do
        createDirectory (unpack wd)
        code <- spawnProcess SpawnedProcessArgs
            { workDir = bd
            , executable = prependIfRelative appd (makePath cf)
            , execArgs = fromList [target cf]
            , outputFile = log
            , awaitExit = True
            }
        checkSpawnSuccess "tier1" code log
    else do
        let mockLog = mockOutput cf
        copyFile (unpack mockLog) (unpack log)
    return ()

run :: Tier1Input -> IO ()
run (Tier1Input ctx cf) = do
    let tid = taskId ctx
    let db = dbConnection ctx
    let appd = appDir ctx
    qrs <- loadQueries ((queriesDir ctx) <> "queries-tier1.sql")
    rid <- dbWithSyncTransaction db (
        createDbEntry db qrs tid )
    spawnMakeAndWait cf appd
    dbWithSyncTransaction db (
        finalizeDbEntry db qrs rid )
    return ()
