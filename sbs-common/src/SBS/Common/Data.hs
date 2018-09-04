
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Data
    ( DyLoadArgs(..)
    , Empty(..)
    , DBConfig(..)
    ) where

import Prelude ()
import qualified Data.Aeson as Aeson

import SBS.Common.Prelude

data Empty = Empty
    deriving (Generic, Show, Typeable)
instance ToJSON Empty where
    toJSON _ = Aeson.object []
instance FromJSON Empty where
    parseJSON _ = return Empty

data DyLoadArgs = DyLoadArgs
    { name :: Text
    } deriving (Generic, Show, Typeable)
instance ToJSON DyLoadArgs

data DBConfig = DBConfig
    { connectionHandle :: Int64
    , runId :: Int64
    } deriving (Generic, Show, Typeable)
instance FromJSON DBConfig
instance ToJSON DBConfig
