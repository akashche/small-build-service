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
    , updateJobState
    , finalizeJob
    , saveResults
    , loadResults
    ) where

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Queries
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
    dbExecute db (get qrs "updateJobState") (object
        [ "id" .= jid
        , "state" .= showText st
        ])

finalizeJob :: DBConnection -> Queries -> Int64 -> State -> Int -> IO ()
finalizeJob db qrs jid st tt = do
    curdate <- getCurrentTime
    dbExecute db (get qrs "updateJobFinish") (object
        [ "id" .= jid
        , "state" .= showText st
        , "finishDate" .= formatISO8601 curdate
        , "totalTimeSeconds" .= tt
        ])
    return ()

saveResults :: DBConnection -> Queries -> Int64 -> Results -> IO ()
saveResults db qrs jid res =
    mapM_ fun (benchmarks (res :: Results))
    where
        fun bench = do
            dbExecute db (get qrs "updateResultsSeq") Empty
            (IncrementedSeq idx) <- dbQueryObject db (get qrs "selectNewResultId") Empty
            dbExecute db (get qrs "insertResult") (object
                [ "id" .= idx
                , "name" .= name (bench :: BenchResult)
                , "mode" .= showText (mode bench)
                , "counts" .= count bench
                , "score" .= score bench
                , "error" .= errored bench
                , "units" .= showText (units bench)
                , "jobId" .= jid
                ])

loadResults :: DBConnection -> Queries -> Int64 -> IO (Vector BenchResult)
loadResults db qrs tid = do
    vec <- dbQueryList db (get qrs "selectResultsByTaskId") (object
        [ "taskId" .= tid
        ]) :: IO (Vector Object)
    return (Vector.map mapper vec)
    where
        mapper val = BenchResult
            { name = jsonGet val "name" :: Text
            , mode = read (jsonGet val "mode") :: BenchMode
            , count = jsonGet val "counts" :: Int
            , score = jsonGet val "score" :: Int
            , errored = jsonGet val "error" :: Int
            , units = read (jsonGet val "units") :: BenchUnit
            }
