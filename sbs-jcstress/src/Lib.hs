
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
import SBS.Common.JCStress
import SBS.Common.Queries
import SBS.Common.Utils
import SBS.Common.Wilton

import Data
import Diff
import Parser

parseOutput :: Text -> IO JCStressResults
parseOutput path =
    withFileText path fun
    where
        fun tx = return (parseJCStressOutput tx path)

createDbEntry :: DBConnection -> Queries -> Int64 -> IO Int64
createDbEntry db qrs tid = do
    dbExecute db (get qrs "updateRunsId") Empty
    (IncrementedSeq idx) <- dbQueryObject db (get qrs "selectRunsId") Empty
    curdate <- getCurrentTime
    dbExecute db (get qrs "insertRun") (object
        [ "id" .= idx
        , "startDate" .= formatISO8601 curdate
        , "state" .= ("running" :: Text)
        , "taskId" .= tid
        ])
    return idx

spawnProcessAndWait :: JCStressConfig -> Text -> IO Text
spawnProcessAndWait cf jdk = do
    if (enabled cf)
    then do
        code <- wiltoncall "process_spawn" (object
            [ "executable" .= exec
            , "args" .= fromList
                [  ("-Xmx" <> (showText (xmxMemoryLimitMB cf)) <> "M")
                , "-jar", jcstressJarPath cf
                , "-m", mode cf
                ]
            , "outputFile" .= log
            , "awaitExit" .= True
            ]) :: IO Int
        when (0 /= code) (throwSpawnFail code)
    else
        copyFile (unpack (mockOutput cf)) logStr
    return log
    where
        log = "jcstress.log" :: Text
        logStr = (unpack log)
        exec = jdk <> "/bin/java"
        throwSpawnFail code = do
            outex <- doesFileExist logStr
            out <- if outex then readFile logStr else return ""
            errorText ("Error running JCStress,"
                <> " code: [" <> (showText code) <>"]"
                <> " output: [" <> (Text.strip out) <> "]")

finalizeDbEntry :: DBConnection -> Queries -> Int64 -> JCStressResults -> JCStressResultsDiff -> IO ()
finalizeDbEntry db qrs rid res diff = do
    curdate <- getCurrentTime
    dbExecute db (get qrs "updateRunFinish") (object
        [ "id" .= rid
        , "state" .= ("finished" :: Text)
        , "finishDate" .= formatISO8601 curdate
        , "passed" .= passedCount res
        , "passedDiff" .= passedDiff diff
        , "interesting" .= Vector.length (interesting res)
        , "interestingDiff" .= interestingDiff diff
        , "failed" .= Vector.length (failed res)
        , "failedDiff" .= failedDiff diff
        , "error" .= Vector.length (error res)
        , "errorDiff" .= errorDiff diff
        ])
    return ()

run :: JCStressInput -> IO ()
run input = do
    let cf = jcstressConfig input
    let db = dbConnection input
    let qrs = queries input
    rid <- dbWithSyncTransaction db (
        createDbEntry db qrs (taskId input))
    log <- spawnProcessAndWait cf (jdkImageDir input)
    res <- parseOutput log
    bl <- parseOutput (baselineOutput cf)
    let diff = diffResults bl res
    dbWithSyncTransaction db ( do
        finalizeDbEntry db qrs rid res diff )
    return ()

