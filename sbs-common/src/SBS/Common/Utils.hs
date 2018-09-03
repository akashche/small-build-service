
{-# LANGUAGE DeriveDataTypeable #-}
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
    ) where

import Prelude ()
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazyEncoding
import qualified Prelude as Prelude
import qualified System.IO as SystemIO

import SBS.Common.Prelude

-- prelude text

errorText :: Text -> a
errorText = Prelude.error . unpack

showText :: Show a => a -> Text
showText = pack . Prelude.show

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
