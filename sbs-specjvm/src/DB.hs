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
import VtUtils.Prelude

import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.Wilton

import Data

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

updateJobState :: DBConnection -> Queries -> Int64 -> State -> IO ()
updateJobState db qrs jid st =
    dbExecute db (mapGet qrs "updateJobState") (object
        [ "id" .= jid
        , "state" .= textShow st
        ])

finalizeJob :: DBConnection -> Queries -> Int64 -> State -> Int -> IO ()
finalizeJob db qrs jid st tt = do
    curdate <- getCurrentTime
    dbExecute db (mapGet qrs "updateJobFinish") (object
        [ "id" .= jid
        , "state" .= textShow st
        , "finishDate" .= dateFormatISO8601 curdate
        , "totalTimeSeconds" .= tt
        ])
    return ()

saveResults :: DBConnection -> Queries -> Int64 -> Results -> IO ()
saveResults db qrs jid res =
    mapM_ fun (benchmarks (res :: Results))
    where
        fun bench = do
            dbExecute db (mapGet qrs "updateResultsSeq") Empty
            (IncrementedSeq idx) <- dbQueryObject db (mapGet qrs "selectNewResultId") Empty
            dbExecute db (mapGet qrs "insertResult") (object
                [ "id" .= idx
                , "name" .= name (bench :: BenchResult)
                , "mode" .= textShow (mode bench)
                , "counts" .= count bench
                , "score" .= score bench
                , "error" .= errored bench
                , "units" .= textShow (units bench)
                , "jobId" .= jid
                ])

loadResults :: DBConnection -> Queries -> Int64 -> IO (Vector BenchResult)
loadResults db qrs tid = do
    vec <- dbQueryList db (mapGet qrs "selectResultsByTaskId") (object
        [ "taskId" .= tid
        ]) :: IO (Vector Value)
    return (mapper <$> vec)
    where
        mapper val = BenchResult
            { name = jsonGet val "name" :: Text
            , mode = read (jsonGet val "mode") :: BenchMode
            , count = jsonGet val "counts" :: Int
            , score = jsonGet val "score" :: Int
            , errored = jsonGet val "error" :: Int
            , units = read (jsonGet val "units") :: BenchUnit
            }
