
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
import SBS.Common.Queries
import SBS.Common.SpecJVM
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

loadModules :: IO ()
loadModules = do
    Prelude.mapM_ load ((
        [ "wilton_db"
        , "wilton_channel"
        , "wilton_process"
        , "sbs_specjvm"
        ]) :: [Text])
    where
        load mod = wiltoncall "dyload_shared_library" (args mod) :: IO ()
        args name = object ["name" .= name]

openDb :: Config -> IO DBConnection
openDb cf =
    if enabled (dbc :: CreateDbConfig)
    then do
        drop
        db <- open
        create db
        return db
    else do
        db <- open
        return db
    where
        dbc = createDb cf
        dbPath = (dbFilePath cf)
        dbPathStr = unpack dbPath
        open = dbOpen ("sqlite://" <> dbPath)
        drop = do
            exists <- doesFileExist dbPathStr
            when (exists) (removeFile dbPathStr)
        create db = dbExecuteFile db (ddlPath dbc)

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
                , "repository" .= ("" :: Text)
                , "revision" .= ("" :: Text)
                , "comment" .= ("created from command line invocation" :: Text)
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

runSpecJVM :: Config -> DBConnection -> Int64 -> IO ()
runSpecJVM cf db tid = do
    qrs <- loadQueries ((queriesDir cf) <> "queries-specjvm.sql")
    wiltoncall "sbs_specjvm_run" (SpecJVMInput
        { taskId = tid
        , dbConnection = db
        , jdkImageDir = (jdkImageDir (cf :: Config))
        , queries = qrs
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
    qrs <- loadQueries ((queriesDir cf) <> "queries-main.sql")
    -- create task
    tid <- createTask db qrs
    -- run specjvm
    runSpecJVM cf db tid
    -- finalize
    finalizeTask db qrs tid

    putStrLn "Run finished"
    return ()
