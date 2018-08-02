
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( start
    ) where

import Prelude
    ( Either(Left, Right), IO, Maybe(Just, Nothing), Show
    , error, id, return, show
    )
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as Aeson (Value(Array, Object, String))
import Data.ByteString (ByteString)
import Data.Data (Data, Typeable)
import Data.Either (Either(Left, Right))
import qualified Data.HashMap.Strict as Map (fromList)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (putStrLn)
import Data.Vector (Vector)
import qualified Data.Vector as Vector (fromList)
import Foreign.Wilton.FFI (invokeWiltonCall)
import GHC.Generics (Generic)

import SBS.Common.Data (DyLoadArgs(..), Empty(..))
import SBS.Common.JCStress (MyObjOut)
import SBS.Common.Utils (bytesToString)


start :: Vector Text -> IO Empty
start arguments = do
    putStrLn (pack (show arguments))
    -- load jcstress module
    resEither <- invokeWiltonCall "dyload_shared_library" (DyLoadArgs "sbs_jcstress")
    case resEither of
        Left err -> error (bytesToString err)
        Right (obj :: Maybe Empty) -> return ()
    -- call jcstress module
    callEither <- invokeWiltonCall "sbs_jcstress_hello" Empty
    case callEither of
        Left err -> error (bytesToString err)
        Right (obj :: Maybe MyObjOut) -> do
            putStrLn (pack (show obj))
            return ()
    return Empty
