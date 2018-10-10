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

module SBS.Common.Utils
    (
    -- prelude text
      showText
    -- file IO
    , withFileBytes
    , withFileText
    -- json
    , decodeJsonFile
    , decodeJsonText
    , encodeJsonText
    , jsonGet
    -- map
    , get
    -- datetime
    , formatDate
    , formatISO8601
    , parseISO8601
    -- paths
    , pathIsAbsolute
    , pathConcat
    , pathPrepend
    -- FS
    , copyDirectory
    ) where

import Prelude ()
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as AesonTypes
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazyEncoding
import qualified Data.Time.Format as TimeFormat
import qualified Prelude as Prelude
import qualified System.IO as SystemIO

import SBS.Common.Prelude

-- prelude text

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
    SystemIO.withBinaryFile (unpack path) SystemIO.ReadMode cb
    where
        cb ha = do
            bs <- ByteStringLazy.hGetContents ha
            res <- fun bs
            return res

withFileText :: Text -> (TextLazy.Text -> IO a) -> IO a
withFileText path fun =
    withFileBytes path cb
    where
        cb bs = do
            res <- fun (TextLazyEncoding.decodeUtf8 bs)
            return res

-- json

encodeJsonText :: ToJSON a => a -> Text
encodeJsonText = decodeUtf8 . ByteString.concat . ByteStringLazy.toChunks . Aeson.encode

decodeJsonText :: forall a . (FromJSON a) => Text -> a
decodeJsonText tx =
    case Aeson.eitherDecode bs :: Either String a of
        Left err -> (error . unpack) (
               "Error decoding JSON,"
            <> " message: [" <> pack err <> "]")
        Right res -> res
    where
        bs = ByteStringLazy.fromChunks [encodeUtf8 tx]

decodeJsonFile :: forall a . (FromJSON a) => Text -> IO a
decodeJsonFile path =
    withFileBytes path fun
    where
        fun bs =
            case Aeson.eitherDecode bs :: Either String a of
                Left err -> (error . unpack) (
                       "Error decoding JSON,"
                    <> " path: [" <> path <> "]"
                    <> " message: [" <> pack err <> "]")
                Right res -> return res

jsonGet :: forall a . (FromJSON a) => Object -> Text -> a
jsonGet obj fieldName =
    case AesonTypes.parseEither (AesonTypes..: fieldName) obj :: Either String a of
        Left err -> (error . unpack) (
                "Error accessing field,"
             <> " name: [" <> fieldName <> "],"
             <> " object: [" <> (encodeJsonText obj) <> "]"
             <> " message: [" <> (pack err) <> "]")
        Right a -> a

-- map

-- get :: (Eq k, Hashable k) => HashMap k v -> k -> v
get :: HashMap Text v -> Text -> v
get map key =
    case lookup key map of
        Just res -> res
        Nothing -> (error . unpack) ("Map entry not found, key: [" <> key <> "]")

-- datetime
formatDate :: Text -> UTCTime -> Text
formatDate format tm =
    pack (TimeFormat.formatTime locale (unpack format) tm)
    where
        locale = TimeFormat.defaultTimeLocale

formatISO8601 :: UTCTime -> Text
formatISO8601 tm = formatDate "%Y-%m-%d %H:%M:%S" tm

parseISO8601 :: Text -> UTCTime
parseISO8601 tx =
    case TimeFormat.parseTimeM False locale iso (unpack tx) :: Maybe UTCTime of
        Just tm -> tm
        Nothing -> (error . unpack) ("Error parsing ISO8601 format, date: [" <> tx <> "]")
    where
        locale = TimeFormat.defaultTimeLocale
        iso = "%Y-%m-%d %H:%M:%S"

-- paths
pathIsAbsolute :: Text -> Bool
pathIsAbsolute path
    | 0 == Text.length path = False
    | '/' == Text.head path = True
    | 1 == Text.length path = False
    | Char.isAlphaNum (Text.head path) && ':' == (Text.index path 1) = True
    | otherwise = False

pathConcat :: Text -> Text -> Text
pathConcat prefix postfix
    | pathIsAbsolute postfix = (error . unpack)
           "Invalid path concatenation with abs postfix,"
        <> " prefix: [" <> prefix <>"]"
        <> " postfix: [" <> postfix <>"]"
    | 0 == Text.length prefix = postfix
    | 0 == Text.length postfix = prefix
    | '/' == Text.last prefix = prefix <> postfix
    | otherwise = prefix <> "/" <> postfix

pathPrepend :: Text -> Text -> Text
pathPrepend prefix path =
    if pathIsAbsolute path then path else pathConcat prefix path

-- FS

copyDirectory :: Text -> Text -> IO ()
copyDirectory src dest = do
    srcex <- doesDirectoryExist (unpack src)
    unless (srcex) (
        (error . unpack) ("Source directory does not exist, src: [" <> src <> "]"))
    destex <- doesDirectoryExist (unpack dest)
    when (destex) (
        (error . unpack) ("Dest directory already exists, dest: [" <> dest <> "]"))
    createDirectory (unpack dest)
    children <- listDirectory (unpack src)
    mapM_ mapper children
    where
        mapper childst = do
            let child = pack childst
            let srcpath = pathConcat src child
            let destpath = pathConcat dest child
            isdir <- doesDirectoryExist (unpack srcpath)
            if isdir
            then copyDirectory srcpath destpath
            else copyFile (unpack srcpath) (unpack destpath)
