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
    ( createJob
    , updateJobState
    , finalizeJob
    , spawnTestsAndWait
    , saveResults
    , extractSummary
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.Tier1
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

createJob :: DBConnection -> Queries -> Int64 -> IO Int64
createJob db qrs tid = do
    dbExecute db (get qrs "updateJobsSeq") Empty
    (IncrementedSeq idx) <- dbQueryObject db (get qrs "selectNewJobId") Empty
    curdate <- getCurrentTime
    dbExecute db (get qrs "insertJob") (object
        [ "id" .= idx
        , "startDate" .= formatISO8601 curdate
        , "state" .= showText StateCreated
        , "taskId" .= tid
        ])
    return idx

updateJobState :: DBConnection -> Queries -> Int64 -> State -> IO ()
updateJobState db qrs jid st =
    dbExecute db (get qrs "updateRunFinish") (object
        [ "id" .= jid
        , "state" .= showText st
        ])

finalizeJob :: DBConnection -> Queries -> Int64 -> State -> IO ()
finalizeJob db qrs jid st = do
    curdate <- getCurrentTime
    dbExecute db (get qrs "updateJobFinish") (object
        [ "id" .= jid
        , "finishDate" .= formatISO8601 curdate
        , "state" .= showText st
        ])

spawnTestsAndWait :: Tier1Config -> Text -> IO ()
spawnTestsAndWait cf appd = do
    let wd = prependIfRelative appd (workDir (cf :: Tier1Config))
    let log = wd <> "tier1.log"
    createDirectory (unpack wd)
    _code <- spawnProcess SpawnedProcessArgs
        { workDir = prependIfRelative appd (buildDir cf)
        , executable = prependIfRelative appd (makePath cf)
        , execArgs = fromList [target cf]
        , outputFile = log
        , awaitExit = True
        }
    -- todo: add less strict check
    -- checkSpawnSuccess "tier1" code log
    return ()

saveResults :: DBConnection -> Queries -> Int64 -> Tier1Results -> IO ()
saveResults _db _qrs _jid _results = do
    return ()

extractSummary :: Text -> Text -> IO ()
extractSummary _logPath _destPath = do
    return ()
