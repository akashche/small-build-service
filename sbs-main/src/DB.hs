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
    ( openDbConnection
    , createTask
    , finalizeTask
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

openDbConnection :: Config -> IO DBConnection
openDbConnection cf =
    if reCreateDb dbc
    then do
        drop
        db <- open
        create db
        return db
    else do
        db <- open
        return db
    where
        sbsc = sbs cf
        appd = appDir (sbsc :: SBSConfig)
        dbc = database sbsc
        dbPath = prependIfRelative appd (dbFilePath dbc)
        qdir = queriesDir (dbc :: DBConfig)
        ddlPath = (prependIfRelative appd qdir) <> "schema.sql"
        dbPathStr = unpack dbPath
        open = dbOpen ("sqlite://" <> dbPath)
        drop = do
            exists <- doesFileExist dbPathStr
            when (exists) (removeFile dbPathStr)
        create db = dbExecuteFile db ddlPath

createTask :: DBConnection -> Queries -> IO Int64
createTask db qrs = do
    idx <- dbWithSyncTransaction db work
    return idx
    where
        work = do
            dbExecute db (get qrs "tasksUpdateId") Empty
            (IncrementedSeq idx) <- dbQueryObject db (get qrs "tasksSelectId") Empty
            curdate <- getCurrentTime
            dbExecute db (get qrs "tasksInsert") (object
                [ "id" .= idx
                , "startDate" .= formatISO8601 curdate
                , "state" .= ("running" :: Text)
                , "comment" .= ("" :: Text)
                ])
            return idx

finalizeTask :: DBConnection -> Queries -> Int64 -> IO ()
finalizeTask db qrs tid = do
    dbWithSyncTransaction db work
    return ()
    where
        work = do
            curdate <- getCurrentTime
            dbExecute db (get qrs "tasksUpdateFinish") (object
                [ "id" .= tid
                , "state" .= ("finished" :: Text)
                , "finishDate" .= formatISO8601 curdate
                ])

