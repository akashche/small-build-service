
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Utils
    ( bytesToString
    , encodeJsonToText
    , errorText
    , showText
    , withFileText
    ) where

import Prelude ()
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as ByteStringLazy
import qualified Data.Text.Lazy as TextLazy
import qualified Data.Text.Lazy.Encoding as TextLazyEncoding
import qualified System.IO as SystemIO

import SBS.Common.Prelude

bytesToString :: ByteString -> String
bytesToString = unpack . decodeUtf8

encodeJsonToText :: ToJSON a => a -> Text
encodeJsonToText = decodeUtf8 . ByteString.concat . ByteStringLazy.toChunks . Aeson.encode

withFileText :: Text -> (TextLazy.Text -> IO a) -> IO a
withFileText path fun =
    SystemIO.withBinaryFile (unpack path) SystemIO.ReadMode (\ha -> do
        bs <- ByteStringLazy.hGetContents ha
        let te = TextLazyEncoding.decodeUtf8 bs
        res <- fun te
        return res )

errorText :: Text -> a
errorText = error . unpack

showText :: Show a => a -> Text
showText = pack . show
