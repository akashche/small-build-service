
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Prelude
    ( Int, IO, Maybe(Just, Nothing), Either(Left, Right), Show, String
    , (+), (-), (*), (/), (>), (<), (==), (/=), (.), (>>), (>>=)
    , div, error, map, read, return, show
    -- Control.Monad
    , unless, when
    -- Data.Aeson
    , FromJSON, ToJSON
    , parseJSON, toJSON
    -- Data.ByteString
    , ByteString
    -- Data.Data
    , Data, Typeable
    -- Data.HashMap.Strict
    , HashMap
    -- Data.Maybe
    , isJust
    -- Data.Monoid
    , (<>)
    -- Data.Text
    , Text
    , pack, unpack
    -- Data.Text.Encoding
    , decodeUtf8, encodeUtf8
    -- Data.Text.IO
    , putStrLn
    -- Data.Text.Lazy.Builder
    , Builder, fromText, fromString, toLazyText
    -- Data.Vector
    , Vector
    , fromList
    -- Foreign.C.String
    , CString
    -- Foreign.Wilton.FFI
    , createWiltonError, invokeWiltonCall, registerWiltonCall
    -- GHC.Generics
    , Generic
    ) where

import Prelude
    ( Int, IO, Maybe(Just, Nothing), Either(Left, Right), Show, String
    , (+), (-), (*), (/), (>), (<), (==), (/=), (.), (>>), (>>=)
    , div, error, map, read, return, show
    )
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.ByteString (ByteString)
import Data.Data (Data, Typeable)
import Data.HashMap.Strict (HashMap)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (putStrLn)
import Data.Text.Lazy.Builder (Builder, fromText, fromString, toLazyText)
import Data.Vector (Vector, fromList)
import Foreign.C.String (CString)
import Foreign.Wilton.FFI (createWiltonError, invokeWiltonCall, registerWiltonCall)
import GHC.Generics (Generic)

