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

module DB
    ( createJob
    , updateJobRepo
    , updateJobState
    , finalizeJob
    ) where

import Prelude ()
import VtUtils.Prelude

import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.Wilton

createJob :: DBConnection -> Queries -> Int64 -> IO Int64
createJob db qrs tid = do
    dbExecute db (mapGet qrs "updateJobsSeq") Empty
    (IncrementedSeq idx) <- dbQueryObject db (mapGet qrs "selectNewJobId") Empty
    curdate <- getCurrentTime
    dbExecute db (mapGet qrs "insertJob") (object
        [ "id" .= idx
        , "startDate" .= dateFormatISO8601 curdate
        , "state" .= textShow StateCreated
        , "taskId" .= tid
        ])
    return idx

updateJobRepo :: DBConnection -> Queries -> Int64 -> Text -> Text -> IO ()
updateJobRepo db qrs jid repo rev = do
    dbExecute db (mapGet qrs "updateJobRepo") (object
        [ "id" .= jid
        , "repository" .= repo
        , "revision" .= rev
        ])

updateJobState :: DBConnection -> Queries -> Int64 -> State -> IO ()
updateJobState db qrs jid st = do
    dbExecute db (mapGet qrs "updateJobState") (object
        [ "id" .= jid
        , "state" .= textShow st
        ])

finalizeJob :: DBConnection -> Queries -> Int64 -> State -> IO ()
finalizeJob db qrs jid st = do
    curdate <- getCurrentTime
    dbExecute db (mapGet qrs "updateJobFinish") (object
        [ "id" .= jid
        , "finishDate" .= dateFormatISO8601 curdate
        , "state" .= textShow st
        ])
