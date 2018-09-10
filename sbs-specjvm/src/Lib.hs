
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Lib
    ( run
    ) where

import Prelude ()
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.SpecJVM
import SBS.Common.Utils
import SBS.Common.Wilton

import Data
import Parser

parseLog :: Text -> IO SpecJVMResults
parseLog path =
    withFileText path fun
    where
        fun tx = return (parseSpecJVMOutput tx path)

createDbEntry :: DBConnection -> Queries -> Int64 -> IO Int64
createDbEntry db qrs tid = do
    idx <- dbWithSyncTransaction db work
    return idx
    where
        work = do
            dbExecute db (get qrs "updateId") Empty
            (IncrementedSeq idx) <- dbQueryObject db (get qrs "selectId") Empty
            curdate <- getCurrentTime
            dbExecute db (get qrs "insert") (object
                [ "id" .= idx
                , "startDate" .= formatISO8601 curdate
                , "state" .= ("running" :: Text)
                , "taskId" .= tid
                ])
            return idx

spawnProcessAndWait :: SpecJVMConfig -> Text -> IO Text
spawnProcessAndWait cf jdk = do
    _ <- wiltoncall "process_spawn" (object
        [ "executable" .= exec
        , "args" .= args
        , "outputFile" .= ("specjvm.log" :: Text)
        , "awaitExit" .= True
        ]) :: IO Int
    return "specjvm.log"
    where
        exec = jdk <> "/bin/java"
        args = fromList
            [  ("-Xmx" <> (showText (xmxMemoryLimitMB cf)) <> "MB")
            , "-jar"
            , specjvmJarPath cf
            , "-t"
            , (showText (threadsCount cf))
            , "-e"
            , exreg (excludedBenchmarks cf)
            ]
        sepNonEmpty st = if Text.length st > 0 then st <> "|" else st
        folder ac el = (sepNonEmpty ac) <> (Text.replace "." "\\." el)
        exreg vec = Vector.foldl' folder "" vec

finalizeDbEntry :: DBConnection -> Queries -> Int64 -> Int -> IO ()
finalizeDbEntry db qrs sid totalTime = do
    dbWithSyncTransaction db work
    return ()
    where
        work = do
            curdate <- getCurrentTime
            dbExecute db (get qrs "updateFinish") (object
                [ "id" .= sid
                , "state" .= ("finished" :: Text)
                , "finishDate" .= formatISO8601 curdate
                , "totalTimeSeconds" .= totalTime
                ])

run :: SpecJVMInput -> IO ()
run input = do
    let db = dbConnection input
    let qrs = queries input
    sid <- createDbEntry db qrs (taskId input)
    log <- spawnProcessAndWait (specjvmConfig input) (jdkImageDir input)
    res <- parseLog log
    finalizeDbEntry db qrs sid (totalTimeSeconds res)
    return ()
