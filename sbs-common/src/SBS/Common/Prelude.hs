
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Prelude
    ( Bool, Int, IO, Maybe(Just, Nothing), Either(Left, Right), Show, String
    , (+), (-), (*), (/), (>), (<), (==), (/=), (.), (>>), (>>=), (&&), (||)
    , div, flip, not, read, return, undefined
    -- Control.Concurrent.MVar
    , MVar
    , newMVar, putMVar, takeMVar
    -- Control.Exception
    , SomeException
    , catch, throw
    -- Control.Monad
    , unless, when
    -- Data.Aeson
    , FromJSON, Object, ToJSON
    , (.=), (.:), (.:?), (.!=)
    , object, parseJSON, toJSON
    -- Data.ByteString
    , ByteString
    -- Data.HashMap.Strict
    , HashMap
    , lookup
    -- Data.Int
    , Int64
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
    , appendFile, putStrLn, readFile, writeFile
    -- Data.Text.Lazy.Builder
    , Builder, fromText, fromString, toLazyText
    -- Data.Typeable
    , Typeable
    , cast
    -- Data.Vector
    , Vector
    , (!)
    , fromList, toList
    -- Debug,Trace
    , trace
    -- Foreign.C.String
    , CString
    -- Foreign.Wilton.FFI
    , createWiltonError, invokeWiltonCall, invokeWiltonCallByteString, registerWiltonCall
    -- GHC.Generics
    , Generic
    ) where

import Prelude
    ( Bool, Int, IO, Maybe(Just, Nothing), Either(Left, Right), Show, String
    , (+), (-), (*), (/), (>), (<), (==), (/=), (.), (>>), (>>=), (&&), (&&), (||)
    , div, flip, not, read, return, undefined
    )
import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, catch, throw)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON, Object, ToJSON, (.=), (.:), (.:?), (.!=), object, parseJSON, toJSON)
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap, lookup)
import Data.Int (Int64)
import Data.Maybe (isJust)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Text.IO (appendFile, putStrLn, readFile, writeFile)
import Data.Text.Lazy.Builder (Builder, fromText, fromString, toLazyText)
import Data.Typeable (Typeable, cast)
import Data.Vector (Vector, (!), fromList, toList)
import Debug.Trace (trace)
import Foreign.C.String (CString)
import Foreign.Wilton.FFI (createWiltonError, invokeWiltonCall, invokeWiltonCallByteString, registerWiltonCall)
import GHC.Generics (Generic)

