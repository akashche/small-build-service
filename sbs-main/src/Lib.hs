
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
import SBS.Common.Data
import SBS.Common.SpecJVM
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

loadModules :: Vector Text -> IO ()
loadModules vec = do
    Vector.mapM_ fun vec
    where
        fun nm = wiltoncall "dyload_shared_library" (DyLoadArgs nm) :: IO ()

-- todo: removeme
data FooBar = FooBar
    { foo :: Text
    , bar :: Int
    } deriving (Generic, Show, Typeable)
instance ToJSON FooBar
instance FromJSON FooBar
_FooBar :: FooBar -> IO ()
_FooBar x = do
    let _ = foo x
    let _ = bar x
    return ()

start :: Vector Text -> IO ()
start arguments = do
    -- check arguments
    when (1 /= Vector.length arguments)
        (errorText "Path to config must be specified as a first and only argument")
    -- load config
    cf <- decodeJsonFile (Vector.head arguments) :: IO Config
    -- load modules
    _ <- loadModules (fromList [
        "wilton_db",
        "wilton_channel",
        "sbs_specjvm"])
    -- openDB connection
    let dbPath = dbFilePath cf
    db <- dbOpen ("sqlite://" <> dbPath)
    -- create run in DB
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

    putStrLn "Run finished"
    return ()
