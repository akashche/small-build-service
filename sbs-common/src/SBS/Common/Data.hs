
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Data
    ( Empty(..)
    , DBConfig(..)
    , IncrementedSeq(..)
    ) where

import Prelude ()
import qualified Data.Aeson as Aeson

import SBS.Common.Prelude

data Empty = Empty
    deriving (Generic, Show)
instance ToJSON Empty where
    toJSON _ = Aeson.object []
instance FromJSON Empty where
    parseJSON _ = return Empty

data DBConfig = DBConfig
    { connectionHandle :: Int64
    , runId :: Int64
    } deriving (Generic, Show)
instance FromJSON DBConfig
instance ToJSON DBConfig

data IncrementedSeq = IncrementedSeq
    { id :: Int64
    } deriving (Generic, Show)
instance FromJSON IncrementedSeq
