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
    ( start
    ) where

import Prelude ()
import qualified Prelude
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JCStress
import SBS.Common.JDKBuild
import SBS.Common.Queries
import SBS.Common.SpecJVM
import SBS.Common.Tier1
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

loadModules :: IO ()
loadModules = do
    Prelude.mapM_ load ((
        [ "wilton_db"
        , "wilton_channel"
        , "wilton_process"
        , "sbs_jcstress"
        , "sbs_jdkbuild"
        , "sbs_specjvm"
        , "sbs_tier1"
        ]) :: [Text])
    where
        load mod = wiltoncall "dyload_shared_library" (args mod) :: IO ()
        args name = object ["name" .= name]

openDb :: Config -> IO DBConnection
openDb cf =
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

runJDKBuild :: Config -> TaskContext -> IO JDKBuildOutput
runJDKBuild cf ctx = do
    res <- wiltoncall "sbs_jdkbuild_run" (JDKBuildInput
        { taskCtx = ctx
        , jdkbuildConfig = (jdkbuild cf)
        })
    return res

runTier1 :: Config -> TaskContext -> IO ()
runTier1 cf ctx = do
    wiltoncall "sbs_tier1_run" (Tier1Input
        { taskCtx = ctx
        , tier1Config = (tier1 cf)
        }) :: IO ()
    return ()

runJCStress :: Config -> TaskContext -> JDKBuildOutput -> IO ()
runJCStress cf ctx jdk = do
    wiltoncall "sbs_jcstress_run" (JCStressInput
        { taskCtx = ctx
        , jdkImageDir = (jdkImageDir (jdk :: JDKBuildOutput))
        , jcstressConfig = (jcstress cf)
        }) :: IO ()

runSpecJVM :: Config -> TaskContext -> JDKBuildOutput -> IO ()
runSpecJVM cf ctx jdk = do
    wiltoncall "sbs_specjvm_run" (SpecJVMInput
        { taskCtx = ctx
        , jdkImageDir = (jdkImageDir (jdk :: JDKBuildOutput))
        , specjvmConfig = (specjvm cf)
        }) :: IO ()

start :: Vector Text -> IO ()
start arguments = do
    -- check arguments
    when (1 /= Vector.length arguments)
        (errorText "Path to config must be specified as a first and only argument")
    -- load modules
    loadModules
    -- load config
    cf <- decodeJsonFile (arguments ! 0) :: IO Config
    -- openDB connection
    db <- openDb cf
    let sbsc = sbs cf
    let qdir = queriesDir ((database sbsc) :: DBConfig)
    qrs <- loadQueries (qdir <> "queries-main.sql")
    -- create task
    tid <- createTask db qrs
    let ctx = TaskContext tid db (appDir (sbsc :: SBSConfig)) qdir
    -- run jdkbuild
    jdk <- runJDKBuild cf ctx
    -- run tier1
    runTier1 cf ctx
    -- run jcstress
    runJCStress cf ctx jdk
    -- run specjvm
    runSpecJVM cf ctx jdk
    -- finalize
    finalizeTask db qrs tid

    putStrLn "Run finished"
    return ()
