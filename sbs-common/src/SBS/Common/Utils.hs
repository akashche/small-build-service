
module SBS.Common.Utils
    ( bytesToString
    , encodeJsonToText
    , withFileText
    ) where

import Prelude
    ( IO, String
    , (.)
    , return
    )
import Data.Aeson (ToJSON)
import qualified Data.Aeson as Aeson (encode)
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString (concat)
import qualified Data.ByteString.Lazy as ByteStringLazy (hGetContents, toChunks)
import Data.Text (Text, unpack)
import Data.Text.Encoding (decodeUtf8)
import qualified Data.Text.Lazy as TextLazy (Text, unpack)
import qualified Data.Text.Lazy.Encoding as TextLazyEncoding (decodeUtf8)
import System.IO (IOMode(ReadMode), withBinaryFile)

bytesToString :: ByteString -> String
bytesToString = unpack . decodeUtf8

encodeJsonToText :: ToJSON a => a -> Text
encodeJsonToText = decodeUtf8 . ByteString.concat . ByteStringLazy.toChunks . Aeson.encode

withFileText :: Text -> (TextLazy.Text -> IO a) -> IO a
withFileText path fun =
    withBinaryFile (unpack path) ReadMode (\ha -> do
        bs <- ByteStringLazy.hGetContents ha
        let te = TextLazyEncoding.decodeUtf8 bs
        res <- fun te
        return res )

