
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Data
    ( Empty(..)
    , IncrementedSeq(..)
    , DBConnection(..)
    ) where

import Prelude ()
import qualified Data.Aeson as Aeson

import SBS.Common.Prelude

data Empty = Empty
    deriving (Generic, Show)
instance ToJSON Empty where
    toJSON _ = object []
instance FromJSON Empty where
    parseJSON _ = return Empty

data IncrementedSeq = IncrementedSeq
    { id :: Int64
    } deriving (Generic, Show)
instance FromJSON IncrementedSeq

data DBConnection = DBConnection
    { connectionHandle :: Int64
    , channelHandle :: Int64
    } deriving (Generic, Show)
instance FromJSON DBConnection
instance ToJSON DBConnection

