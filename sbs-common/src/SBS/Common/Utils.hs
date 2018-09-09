
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Utils
    (
    -- prelude text
      errorText
    , showText
    -- file IO
    , withFileBytes
    , withFileText
    -- json
    , decodeJsonFile
    , decodeJsonText
    , encodeJsonText
    , jsonGet
    -- debug
    , debug
    -- map
    , get
    -- datetime
    , formatISO8601
    , parseISO8601
    ) where

import Prelude ()
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazyEncoding
import qualified Data.Time.Format as TimeFormat
import qualified Prelude as Prelude
import qualified System.IO as SystemIO

import SBS.Common.Prelude

-- prelude text

errorText :: Text -> a
errorText = Prelude.error . unpack

showText :: (Show a, Typeable a) => a -> Text
showText val
    | isJust castedText = fromJust castedText
    | isJust castedString = pack (fromJust castedString)
    | isJust castedBytes = decodeUtf8 (fromJust castedBytes)
    | otherwise = pack (Prelude.show val)
    where
        castedText = cast val :: Maybe Text
        castedString = cast val :: Maybe String
        castedBytes = cast val :: Maybe ByteString

-- file IO

withFileBytes :: Text -> (ByteStringLazy.ByteString -> IO a) -> IO a
withFileBytes path fun =
    SystemIO.withBinaryFile (unpack path) SystemIO.ReadMode (\ha -> do
        bs <- ByteStringLazy.hGetContents ha
        res <- fun bs
        return res )

withFileText :: Text -> (TextLazy.Text -> IO a) -> IO a
withFileText path fun =
    withFileBytes path (\bs -> do
        let te = TextLazyEncoding.decodeUtf8 bs
        res <- fun te
        return res )

-- json

encodeJsonText :: ToJSON a => a -> Text
encodeJsonText = decodeUtf8 . ByteString.concat . ByteStringLazy.toChunks . Aeson.encode

decodeJsonText :: forall a . (FromJSON a) => Text -> a
decodeJsonText tx =
    case Aeson.eitherDecode bs :: Either String a of
        Left err -> errorText (pack err)
        Right res -> res
    where
        bs = ByteStringLazy.fromChunks [encodeUtf8 tx]

decodeJsonFile :: forall a . (FromJSON a) => Text -> IO a
decodeJsonFile path =
    withFileBytes path fun
    where
        fun bs =
            case Aeson.eitherDecode bs :: Either String a of
                Left err -> errorText (pack err)
                Right res -> return res

jsonGet :: forall a . (FromJSON a) => Object -> Text -> a
jsonGet obj fieldName =
    case AesonTypes.parseEither (.: fieldName) obj :: Either String a of
        Left err -> errorText ("Error accessing field,"
             <> " name: [" <> fieldName <> "],"
             <> " object: [" <> (encodeJsonText obj) <> "]"
             <> " message: [" <> (pack err) <> "]")
        Right a -> a

-- trace

debug :: a -> Text -> a
debug val msg = trace (unpack msg) val

-- map

-- get :: (Eq k, Hashable k) => HashMap k v -> k -> v
get :: HashMap Text v -> Text -> v
get map key =
    case lookup key map of
        Just res -> res
        Nothing -> errorText ("Map entry not found, key: [" <> key <> "]")

-- datetime
formatISO8601 :: UTCTime -> Text
formatISO8601 tm = pack (TimeFormat.formatTime locale iso tm)
    where
        locale = TimeFormat.defaultTimeLocale
        iso = "%Y-%m-%d %H:%M:%S"

parseISO8601 :: Text -> UTCTime
parseISO8601 tx =
    case TimeFormat.parseTimeM False locale iso (unpack tx) :: Maybe UTCTime of
        Just tm -> tm
        Nothing -> errorText ("Error parsing ISO8601 format, date: [" <> tx <> "]")
    where
        locale = TimeFormat.defaultTimeLocale
        iso = "%Y-%m-%d %H:%M:%S"
