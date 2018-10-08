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

module SBS.Common.Tier1
    ( Tier1Config(..)
    , Tier1Input(..)
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data

data Tier1Config = Tier1Config
    { enabled :: Bool
    , workDir :: Text
    , mockOutputPath :: Text
    , outputFile :: Text
    , summaryFile :: Text
    , buildDir :: Text
    , makePath :: Text
    , target :: Text
    } deriving (Generic, Show)
instance ToJSON Tier1Config
instance FromJSON Tier1Config

data Tier1Input = Tier1Input
    { taskCtx :: TaskContext
    , tier1Config :: Tier1Config
    } deriving (Generic, Show)
instance ToJSON Tier1Input
instance FromJSON Tier1Input
