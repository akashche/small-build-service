
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Wilton
    ( wiltoncall
    -- db
    , DBConnection(..)
    , dbOpen
    , dbExecute
    , dbExecuteFile
    , dbQueryList
    , dbQueryObject
    , dbWithTransaction
    , dbWithSyncTransaction
    , dbClose
    -- fs
    , fsExists
    , fsUnlink
    ) where

import Prelude ()
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import SBS.Common.Parsec
import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Utils

-- wilton access with stack unwinding

wiltoncall ::
    forall arguments result . (ToJSON arguments, FromJSON result, Typeable result) =>
    Text -> arguments -> IO result
wiltoncall callName args = do
    err <- invokeWiltonCall (encodeUtf8 callName) args
    case err of
        Left msg -> errorText (callName <> ": " <> (decodeUtf8 msg))
        Right (res :: result) -> return res

-- DB access

data DBConnection = DBConnection
    { connectionHandle :: Int64
    , channelHandle :: Int64
    } deriving (Generic, Show, Typeable)
instance FromJSON DBConnection
instance ToJSON DBConnection

dbOpen :: Text -> IO DBConnection
dbOpen url = do
    connObj <- wiltoncall "db_connection_open" url :: IO Object
    let connHandle = jsonGet connObj "connectionHandle" :: Int64
    chanObj <- wiltoncall "channel_create" (object
        [ "name" .= url
        , "size" .= (1 :: Int)
        ]) :: IO Object
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
    queries <- withFileText path load
    mapM_ exec queries
    return ()
    where
        liner tx = fromList (Text.splitOn "\n" tx)
        nonBlank tx = Text.length (Text.strip tx) > 0
        nonComment tx = not (Text.isPrefixOf "--" (Text.dropWhile Char.isSpace tx))
        validLine tx = (nonBlank tx) && (nonComment tx)
        filterLines lines = filter validLine lines
        validBucket buck = Vector.length buck > 0
        concatBucket buck = Text.intercalate "\n" (toList buck)
        parser = do
            li <- sepBy1 (many1 (noneOf [';'])) (char ';') :: Parser [String]
            let vec = fromList li
            let buckets = map (filterLines . liner . pack) vec
            let filtered = filter validBucket buckets
            let queries = map concatBucket filtered
            return queries
        exec qr = dbExecute db qr Empty
        load contents =
            case parse parser (unpack path) contents of
                Left err -> errorText (errToText err)
                Right res -> return res

dbQueryList ::
    forall a b . (ToJSON a, FromJSON b, Typeable b) =>
    DBConnection -> Text -> a -> IO (Vector b)
dbQueryList db sqlQuery pars = do
    res <- wiltoncall "db_connection_query" (object
        [ "connectionHandle" .= connectionHandle (db :: DBConnection)
        , "sql" .= sqlQuery
        , "params" .= pars
        ]) :: IO (Vector b)
    return res

dbQueryObject ::
    forall a b . (ToJSON a, FromJSON b, Typeable b) =>
    DBConnection -> Text -> a -> IO b
dbQueryObject db sqlQuery pars = do
    vec <- dbQueryList db sqlQuery pars
    let len = Vector.length vec
    when (1 /= len) (errorText (
               "Invalid number of records returned, expected 1 record,"
            <> " query: [" <> sqlQuery <> "], params: [" <> encodeJsonText(pars) <> "],"
            <> " number of records: [" <> (showText len) <>  "]"))
    return (vec ! 0)

dbWithTransaction :: forall a . DBConnection -> IO a -> IO a
dbWithTransaction db cb = do
    th <- wiltoncall "db_transaction_start" (object
        [ "connectionHandle" .= connectionHandle (db :: DBConnection)
        ]) :: IO Object
    resEither <- catch
        ( do
            res <- cb
            wiltoncall "db_transaction_commit" th :: IO ()
            return (Right res))
        (\(e :: SomeException) -> return (Left e))
    case resEither of
        Left e -> do
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
        ]) :: IO Object
    resEither <- catch
        ( do
            res <- dbWithTransaction db cb
            return (Right res))
        (\(e :: SomeException) -> return (Left e))
    _ <- wiltoncall "channel_receive" (object
        [ "channelHandle" .= channelHandle (db :: DBConnection)
        , "timeoutMillis" .= (0 :: Int)
        ]) :: IO Empty
    case resEither of
        Left e -> throw e
        Right res -> return res

-- FS access
fsExists :: Text -> IO Bool
fsExists path = do
    resObj <- wiltoncall "fs_exists" (object
        [ "path" .= path
        ]) :: IO Object
    let exists = jsonGet resObj "exists" :: Bool
    return exists

fsUnlink :: Text -> IO ()
fsUnlink path =
    wiltoncall "fs_unlink" (object
        [ "path" .= path
        ]) :: IO ()
