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

module SBS.Common.Data
    ( Empty(..)
    , IncrementedSeq(..)
    , DBConnection(..)
    , TaskContext(..)
    , DiffRequest(..)
    , State(..)
    ) where

import Prelude ()
import VtUtils.Prelude

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

data TaskContext = TaskContext
    { taskId :: Int64
    , dbConnection :: DBConnection
    , appDir :: Text
    , queriesDir :: Text
    , destBaseDir :: Text
    , destDir :: Text
    } deriving (Generic, Show)
instance FromJSON TaskContext
instance ToJSON TaskContext

data DiffRequest = DiffRequest
    { taskId1 :: Int64
    , taskId2 :: Int64
    , dbConnection :: DBConnection
    , appDir :: Text
    , queriesDir :: Text
    } deriving (Generic, Show)
instance FromJSON DiffRequest
instance ToJSON DiffRequest

-- JobState

data State =
      StateCreated
    | StateRunning
    | StateSuccess
    | StateError
    deriving (Read, Show)



