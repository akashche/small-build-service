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

module SBS.Common.Wilton
    ( wiltoncall
    -- dyload
    , dyloadModules
    -- db
    , dbOpen
    , dbExecute
    , dbExecuteFile
    , dbQueryList
    , dbQueryObject
    , dbWithTransaction
    , dbWithSyncTransaction
    , dbClose
    -- process
    , SpawnedProcessArgs(..)
    , spawnProcess
    , checkSpawnSuccess
    -- re-export
    , createWiltonError
    , registerWiltonCall
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Vector as Vector
import qualified Foreign.Wilton.FFI as WiltonFFI
import qualified System.Directory as Directory
import qualified Text.Parsec as Parsec

-- re-export
import Foreign.Wilton.FFI (createWiltonError, registerWiltonCall)

import SBS.Common.Data

-- wilton access with stack unwinding

wiltoncall ::
    forall a b . (ToJSON a, FromJSON b) =>
    Text -> a -> IO b
wiltoncall callName callData = do
    err <- WiltonFFI.invokeWiltonCall (encodeUtf8 callName) callData :: IO (Either ByteString b)
    case err of
        Left msg -> (error . unpack) ("WiltonCall error,"
            <> " name: [" <> callName <> "],"
            <> " message: [" <> (decodeUtf8 msg) <> "]")
        Right res -> return res

-- modules load

dyloadModules :: [Text] -> IO ()
dyloadModules mods = do
    mapM_ load mods
    where
        load md = wiltoncall "dyload_shared_library" (args md) :: IO ()
        args name = object ["name" .= name]

-- DB access

dbOpen :: Text -> IO DBConnection
dbOpen url = do
    connObj <- wiltoncall "db_connection_open" url :: IO Value
    let connHandle = jsonGet connObj "connectionHandle" :: Int64
    chanObj <- wiltoncall "channel_create" (object
        [ "name" .= url
        , "size" .= (1 :: Int)
        ]) :: IO Value
    let chanHandle = jsonGet chanObj "channelHandle" :: Int64
    return (DBConnection connHandle chanHandle)

dbClose :: DBConnection -> IO ()
dbClose db = do
    wiltoncall "db_connection_close" (object
        [ "connectionHandle" .= connectionHandle (db :: DBConnection)
        ]) :: IO ()
    wiltoncall "channel_close" (object
        [ "channelHandle" .= channelHandle (db :: DBConnection)
        ]) :: IO ()
    return ()

dbExecute :: forall a . (ToJSON a) => DBConnection -> Text -> a -> IO ()
dbExecute db sqlQuery pars = do
    wiltoncall "db_connection_execute" (object
        [ "connectionHandle" .= connectionHandle (db :: DBConnection)
        , "sql" .= sqlQuery
        , "params" .= pars
        ]) :: IO ()
    return ()

dbExecuteFile :: DBConnection -> Text -> IO ()
dbExecuteFile db path = do
    qrs <- parsecParseFile parser path
    mapM_ exec qrs
    return ()
    where
        liner tx = fromList (Text.splitOn "\n" tx)
        nonBlank tx = Text.length (Text.strip tx) > 0
        nonComment tx = not (Text.isPrefixOf "--" (Text.dropWhile Char.isSpace tx))
        validLine tx = (nonBlank tx) && (nonComment tx)
        filterLines lines = Vector.filter validLine lines
        validBucket buck = Vector.length buck > 0
        concatBucket buck = Text.intercalate "\n" (toList buck)
        parser = do
            li <- Parsec.sepBy1 (Parsec.many1 (Parsec.noneOf [';'])) (Parsec.char ';') :: Parser [String]
            let vec = fromList li
            let buckets = Vector.map (filterLines . liner . pack) vec
            let filtered = Vector.filter validBucket buckets
            let queries = Vector.map concatBucket filtered
            return queries
        exec qr = dbExecute db qr Empty
--         load contents =
--             case parse parser (unpack path) contents of
--                 Left err -> (error . unpack) (errToText err)
--                 Right res -> return res

dbQueryList ::
    forall a b . (ToJSON a, FromJSON b) =>
    DBConnection -> Text -> a -> IO (Vector b)
dbQueryList db sqlQuery pars = do
    res <- wiltoncall "db_connection_query" (object
        [ "connectionHandle" .= connectionHandle (db :: DBConnection)
        , "sql" .= sqlQuery
        , "params" .= pars
        ]) :: IO (Vector b)
    return res

dbQueryObject ::
    forall a b . (ToJSON a, FromJSON b) =>
    DBConnection -> Text -> a -> IO b
dbQueryObject db sqlQuery pars = do
    vec <- dbQueryList db sqlQuery pars
    let len = Vector.length vec
    when (1 /= len) ((error . unpack) (
               "Invalid number of records returned, expected 1 record,"
            <> " query: [" <> sqlQuery <> "], params: [" <> jsonEncodeText(pars) <> "],"
            <> " number of records: [" <> (textShow len) <>  "]"))
    return (vec ! 0)

dbWithTransaction :: forall a . DBConnection -> IO a -> IO a
dbWithTransaction db cb = do
    th <- wiltoncall "db_transaction_start" (object
        [ "connectionHandle" .= connectionHandle (db :: DBConnection)
        ]) :: IO Value
    resEither <- try
        ( do
            res <- cb
            wiltoncall "db_transaction_commit" th :: IO ()
            return res)
    case resEither of
        Left (e :: SomeException) -> do
            wiltoncall "db_transaction_rollback" th :: IO ()
            throw e
        Right res -> return res

-- required for sqlite with multi-threading
dbWithSyncTransaction :: forall a . DBConnection -> IO a -> IO a
dbWithSyncTransaction db cb = do
    _ <- wiltoncall "channel_send" (object
        [ "channelHandle" .= channelHandle (db :: DBConnection)
        , "message" .= ("[]" :: Text)
        , "timeoutMillis" .= (0 :: Int)
        ]) :: IO Value
    resEither <- try
        ( do
            res <- dbWithTransaction db cb
            return res)
    _ <- wiltoncall "channel_receive" (object
        [ "channelHandle" .= channelHandle (db :: DBConnection)
        , "timeoutMillis" .= (0 :: Int)
        ]) :: IO Empty
    case resEither of
        Left (e :: SomeException) -> throw e
        Right res -> return res

data SpawnedProcessArgs = SpawnedProcessArgs
    { workDir :: Text
    , executable :: Text
    , execArgs :: Vector Text
    , outputFile :: Text
    , awaitExit :: Bool
    } deriving (Show)

-- process

spawnProcess :: SpawnedProcessArgs -> IO Int
spawnProcess (SpawnedProcessArgs wd exec args outFile await) = do
    code <- Directory.withCurrentDirectory (unpack wd) (
        wiltoncall "process_spawn" (object
        [ "executable" .= exec
        , "args" .= args
        , "outputFile" .= outFile
        , "awaitExit" .= await
        ]) :: IO Int)
    return code

checkSpawnSuccess :: Text -> Int -> Text -> IO ()
checkSpawnSuccess label code logFile =
    when (0 /= code) (do
        let logStr = unpack logFile
        outex <- Directory.doesFileExist logStr
        out <- if outex then readFile logStr else return ""
        (error . unpack) ("Process spawn error,"
            <> " process: [" <> label <> "],"
            <> " code: [" <> (textShow code) <>"],"
            <> " output: [" <> (Text.take 1024 (Text.strip out)) <> "]"))
