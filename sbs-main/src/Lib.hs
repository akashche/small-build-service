
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Lib
    ( start
    ) where

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

loadModules :: IO ()
loadModules = do
    mapM_ load ((fromList
        [ "wilton_db"
        , "wilton_fs"
        , "wilton_channel"
        , "sbs_specjvm"
        ]) :: Vector Text)
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

start :: Vector Text -> IO ()
start arguments = do
    -- check arguments
    when (1 /= Vector.length arguments)
        (errorText "Path to config must be specified as a first and only argument")
    -- load modules
    loadModules
    -- load config
    cf <- decodeJsonFile (Vector.head arguments) :: IO Config
    -- openDB connection
    _ <- openDb cf
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
