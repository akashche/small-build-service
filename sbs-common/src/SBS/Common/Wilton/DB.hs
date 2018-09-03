
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Wilton.DB
    ( dbOpen
    , dbExecute
    , dbQueryList
    , dbQueryObject
    , dbWithTransaction
    , dbWithSyncTransaction
    , dbClose
    ) where

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Utils
import SBS.Common.Wilton.Call

data DBConnHandle = DBConnHandle
    { connectionHandle :: Int64
    } deriving (Typeable, Data, Generic, Show)
instance FromJSON DBConnHandle
instance ToJSON DBConnHandle

data DBTranHandle = DBTranHandle
    { transactionHandle :: Int64
    } deriving (Typeable, Data, Generic, Show)
instance FromJSON DBTranHandle
instance ToJSON DBTranHandle
_DBTranHandle :: DBTranHandle -> IO ()
_DBTranHandle x = do
    let _ = transactionHandle x
    return ()

data DBQueryArgs a = DBQueryArgs
    { connectionHandle :: Int64
    , sql :: Text
    , params :: a
    } deriving (Typeable, Data, Generic, Show)
instance ToJSON a => ToJSON (DBQueryArgs a)
_DBQueryArgs :: DBQueryArgs a -> IO ()
_DBQueryArgs x = do
    let _ = sql x
    let _ = params x
    return ()

dbOpen :: Text -> IO Int64
dbOpen url = do
    hsObjText <- wiltoncallText "db_connection_open" url
    let haObj = decodeJsonText hsObjText :: DBConnHandle
    return (connectionHandle (haObj :: DBConnHandle))

dbExecute :: forall a . (ToJSON a) => Int64 -> Text -> a -> IO ()
dbExecute handle sqlQuery pars = do
    let args = DBQueryArgs handle sqlQuery pars
    wiltoncall "db_connection_execute" args :: IO ()
    return ()

dbQueryList ::
    forall a b . (ToJSON a, Data b, FromJSON b) =>
    Int64 -> Text -> a -> IO (Vector b)
dbQueryList handle sqlQuery pars = do
    let args = DBQueryArgs handle sqlQuery pars
    res <- wiltoncall "db_connection_query" args :: IO (Vector b)
    return res

dbQueryObject ::
    forall a b . (ToJSON a, Data b, FromJSON b) =>
    Int64 -> Text -> a -> IO b
dbQueryObject handle sqlQuery pars = do
    vec <- dbQueryList handle sqlQuery pars
    let len = Vector.length vec
    when (1 /= len) (errorText (
               "Invalid number of records returned, expected 1 record,"
            <> " query: [" <> sqlQuery <> "], params: [" <> encodeJsonText(pars) <> "],"
            <> " number of records: [" <> (showText len) <>  "]"))
    return (Vector.head vec)

dbWithTransaction :: forall a . Int64 -> IO a -> IO a
dbWithTransaction handle cb = do
    let ha = DBConnHandle handle
    th <- wiltoncall "db_transaction_start" ha :: IO DBTranHandle
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

dbWithSyncTransaction :: forall a . MVar Text -> Int64 -> IO a -> IO a
dbWithSyncTransaction mutex handle cb = do
    mval <- takeMVar mutex
    resEither <- catch
        ( do
            res <- dbWithTransaction handle cb
            return (Right res))
        (\(e :: SomeException) -> return (Left e))
    putMVar mutex mval
    case resEither of
        Left e -> throw e
        Right res -> return res

dbClose :: Int64 -> IO ()
dbClose handle = do
    wiltoncall "db_connection_close" handle :: IO ()
    return ()
