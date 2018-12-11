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
    , updateTaskState
    , finalizeTask
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified System.Directory as Directory

import SBS.Common.Data
import SBS.Common.Queries
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
        dbPath = pathPrepend appd (dbFilePath dbc)
        qdir = queriesDir (dbc :: DBConfig)
        ddlPath = pathConcat (pathPrepend appd qdir) "schema.sql"
        dbPathStr = unpack dbPath
        open = dbOpen ("sqlite://" <> dbPath)
        drop = do
            exists <- Directory.doesFileExist dbPathStr
            when (exists) (Directory.removeFile dbPathStr)
        create db = dbExecuteFile db ddlPath

createTask :: DBConnection -> Queries -> IO Int64
createTask db qrs = do
    dbExecute db (mapGet qrs "updateTasksSeq") Empty
    (IncrementedSeq idx) <- dbQueryObject db (mapGet qrs "selectNewTaskId") Empty
    curdate <- getCurrentTime
    dbExecute db (mapGet qrs "insertTask") (object
        [ "id" .= idx
        , "startDate" .= dateFormatISO8601 curdate
        , "state" .= textShow StateCreated
        , "comment" .= ("" :: Text)
        ])
    return idx

updateTaskState :: DBConnection -> Queries -> Int64 -> State -> IO ()
updateTaskState db qrs tid st = do
    dbExecute db (mapGet qrs "updateTaskState") (object
        [ "id" .= tid
        , "state" .= textShow st
        ])

finalizeTask :: DBConnection -> Queries -> Int64 -> State -> IO ()
finalizeTask db qrs tid st = do
    curdate <- getCurrentTime
    dbExecute db (mapGet qrs "updateTaskFinish") (object
        [ "id" .= tid
        , "state" .= textShow st
        , "finishDate" .= dateFormatISO8601 curdate
        ])

