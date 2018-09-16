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

module Data
    ( DBConfig(..)
    , LoggingConfig(..)
    , SBSConfig(..)
    , Config(..)
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.JCStress
import SBS.Common.JDKBuild
import SBS.Common.SpecJVM
import SBS.Common.Tier1

data DBConfig = DBConfig
    { dbFilePath :: Text
    , queriesDir :: Text
    , reCreateDb :: Bool
    } deriving (Generic, Show)
instance FromJSON DBConfig
instance ToJSON DBConfig

data LoggingConfig = LoggingConfig
    { enabled :: Bool
    } deriving (Generic, Show)
instance FromJSON LoggingConfig
instance ToJSON LoggingConfig

data SBSConfig = SBSConfig
    { appDir :: Text
    , database :: DBConfig
    , logging :: LoggingConfig
    } deriving (Generic, Show)
instance FromJSON SBSConfig
instance ToJSON SBSConfig

data Config = Config
    { sbs :: SBSConfig
    , jdkbuild :: JDKBuildConfig
    , tier1 :: Tier1Config
    , jcstress :: JCStressConfig
    , specjvm :: SpecJVMConfig
    } deriving (Generic, Show)
instance FromJSON Config
instance ToJSON Config
