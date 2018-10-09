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

module SBS.Common.Prelude
    ( Bool(True, False), Either(Left, Right), Int, IO, Maybe(Just, Nothing), Read, Show, String
    , (+), (-), (*), (/), (>), (<), (==), (/=), (.), (>>), (>>=), (&&), (||)
    , div, error, flip, fmap, fromIntegral, id, mapM_, not, otherwise, read, return, show, undefined
    -- Control.Exception
    , SomeException
    , catch, throw
    -- Control.Monad
    , unless, when
    -- Control.Monad.ST
    , runST
    -- Data.Aeson
    , FromJSON, Object, ToJSON
    , (.=)
    , object
    -- Data.ByteString
    , ByteString
    -- Data.HashMap.Strict
    , HashMap
    , lookup
    -- Data.Int
    , Int64
    -- Data.Maybe
    , fromJust, isJust
    -- Data.Monoid
    , (<>)
    -- Data.Text
    , Text
    , pack, unpack
    -- Data.Text.Encoding
    , decodeUtf8, encodeUtf8
    -- Data.Text.IO
    , appendFile, putStrLn, readFile, writeFile
    -- Data.Text.Lazy
    , toStrict
    -- Data.Text.Lazy.Builder
    , Builder, fromText, fromLazyText, fromString, toLazyText
    -- Data.Time.Clock
    , UTCTime
    , getCurrentTime
    -- Data.Typeable
    , Typeable
    , cast
    -- Data.Vector
    , Vector
    , (!)
    , fromList, toList
    -- Debug.Trace
    , trace
    -- Foreign.C.String
    , CString
    -- Foreign.Wilton.FFI
    , createWiltonError, invokeWiltonCall, invokeWiltonCallByteString, registerWiltonCall
    -- GHC.Generics
    , Generic
    -- System.Directory
    , canonicalizePath, createDirectory, createDirectoryIfMissing, copyFile, doesDirectoryExist, doesFileExist
    , findExecutable, getCurrentDirectory, getFileSize, listDirectory
    , removeFile, removeDirectoryRecursive, renameFile, renameDirectory, withCurrentDirectory
    ) where

import Prelude
    ( Bool(True, False), Either(Left, Right), Int, IO, Maybe(Just, Nothing), Read, Show, String
    , (+), (-), (*), (/), (>), (<), (==), (/=), (.), (>>), (>>=), (&&), (&&), (||)
    , div, error, flip, fmap, fromIntegral, id, mapM_, not, otherwise, read, return, show, undefined
    )
import Control.Exception (SomeException, catch, throw)
import Control.Monad (unless, when)
import Control.Monad.ST (runST)
import Data.Aeson (FromJSON, Object, ToJSON, (.=), object)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap, lookup)
import Data.Int (Int64)
import Data.Maybe (fromJust, isJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (appendFile, putStrLn, readFile, writeFile)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (Builder, fromText, fromLazyText, fromString, toLazyText)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Typeable (Typeable, cast)
import Data.Vector (Vector, (!), fromList, toList)
import Debug.Trace (trace)
import Foreign.C.String (CString)
import Foreign.Wilton.FFI (createWiltonError, invokeWiltonCall, invokeWiltonCallByteString, registerWiltonCall)
import GHC.Generics (Generic)
import System.Directory
    ( canonicalizePath, createDirectory, createDirectoryIfMissing, copyFile, doesDirectoryExist, doesFileExist
    , findExecutable, getCurrentDirectory, getFileSize, listDirectory
    , removeFile, removeDirectoryRecursive, renameFile, renameDirectory, withCurrentDirectory )

