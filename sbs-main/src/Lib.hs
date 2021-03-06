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
    ( initApp
    , initTask
    , initResults
    ) where

import Prelude ()
import VtUtils.Prelude

import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.Wilton

import Data
import DB

modules :: [Text]
modules =
        [ "wilton_db"
        , "wilton_channel"
        , "wilton_process"
        , "sbs_jcstress"
        , "sbs_jdkbuild"
        , "sbs_specjvm"
        , "sbs_tier1"
        ]

resolveQueriesDir :: Config -> Text
resolveQueriesDir cf =
    pathPrepend appd qdir
    where
        appd = appDir (sbs cf :: SBSConfig)
        qdir = queriesDir ((database (sbs cf)) :: DBConfig)

initApp :: Vector Text -> IO (Config, DBConnection, Queries)
initApp arguments = do
    when (length arguments < 1)
        (error "Path to the JSON configuration file must be specified as a first argument")
    dyloadModules modules
    cf <- jsonDecodeFile (arguments ! 0) :: IO Config
    db <- openDbConnection cf
    qrs <- queriesLoad (pathConcat (resolveQueriesDir cf) "queries-main.sql")
    return (cf, db, qrs)

initTask :: Config -> DBConnection -> Queries -> IO TaskContext
initTask cf db qrs = do
    tid <- createTask db qrs
    return (TaskContext tid db appd qdir "" "")
    where
        appd = appDir (sbs cf :: SBSConfig)
        qdir = resolveQueriesDir cf

initResults :: Vector Text -> IO (TaskContext, Config, Text)
initResults arguments = do
    when (2 /= length arguments)
        (error "Invalid arguments specified, expected: [path/to/config.json, dest_dir]")
    dyloadModules modules
    cf <- jsonDecodeFile (arguments ! 0) :: IO Config
    curdate <- getCurrentTime
    let postfix = dateFormat "%Y_%m_%d" curdate
    let basedir = pathConcat (arguments ! 1) postfix
    let ctx = TaskContext
            { taskId = 0
            , dbConnection = DBConnection 0 0
            , appDir = appDir (sbs cf :: SBSConfig)
            , queriesDir = ""
            , destBaseDir = basedir
            , destDir = ""
            }
    return (ctx, cf, basedir)