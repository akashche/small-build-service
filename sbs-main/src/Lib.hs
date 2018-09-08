
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
import SBS.Common.Utils
import SBS.Common.Wilton

import Data
import Queries

loadModules :: IO ()
loadModules = do
    Prelude.mapM_ load ((
        [ "wilton_db"
        , "wilton_fs"
        , "wilton_channel"
        , "sbs_specjvm"
        ]) :: [Text])
    where
        load mod = wiltoncall "dyload_shared_library" (args mod) :: IO ()
        args name = object ["name" .= name]

openDb :: Config -> IO DBConnection
openDb cf =
    if enabled dbc
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
        open = dbOpen ("sqlite://" <> dbPath)
        drop = do
            exists <- fsExists dbPath
            when (exists) (fsUnlink dbPath)
        create db = dbExecuteFile db (ddlPath dbc)

createTask :: DBConnection -> (HashMap Text Text) -> IO Int64
createTask db qrs = do
    obj <- dbWithSyncTransaction db work
    return (id obj)
    where
        work = do
            dbExecute db (get qrs "tasksUpdateId") Empty
            res <- dbQueryObject db (get qrs "tasksSelectId") Empty :: IO IncrementedSeq
            return res

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
    queries <- loadQueries (queriesPath cf)
    taskId <- createTask db queries
    putStrLn (showText taskId)
    -- create task in DB
    {--
    dbWithSyncTransaction db ( do
        dbExecute db "create table if not exists t1 (foo varchar, bar int)" Empty
        dbExecute db "insert into t1 values(:foo, :bar)" (FooBar "foo" 42)
        return () )
    vec <- dbWithSyncTransaction db ( do
        vec <- dbQueryList db "select * from t1" Empty :: IO (Vector FooBar)
        return vec )
    putStrLn (showText vec)
    -- make specjvm args
    let dbHandle = connectionHandle (db :: DBConnection)
    let sjvmi = SpecJVMInput (jdkImageDir (cf :: Config)) (DBConfig dbHandle 42) (specjvmConfig cf)
    -- run specjvm
    wiltoncall "sbs_specjvm_run" sjvmi :: IO ()
    --}

    putStrLn "Run finished"
    return ()
