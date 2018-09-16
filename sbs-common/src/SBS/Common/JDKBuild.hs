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

module SBS.Common.JDKBuild
    ( JDKBuildConfig(..)
    , JDKBuildInput(..)
    , JDKBuildOutput(..)
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data

data JDKBuildConfig = JDKBuildConfig
    { enabled :: Bool
    , workDir :: Text
    , mockOutputDir :: Text
    , sourceDir :: Text
    , bootJdkDir :: Text
    , jtregDir :: Text
    , bashPath :: Text
    , hgPath :: Text
    , makePath :: Text
    , logLevel :: Text
    , additionalConfigureArguments :: Vector Text
    , target :: Text
    } deriving (Generic, Show)
instance ToJSON JDKBuildConfig
instance FromJSON JDKBuildConfig

data JDKBuildInput = JDKBuildInput
    { taskCtx :: TaskContext
    , jdkbuildConfig :: JDKBuildConfig
    } deriving (Generic, Show)
instance ToJSON JDKBuildInput
instance FromJSON JDKBuildInput

data JDKBuildOutput = JDKBuildOutput
    { jdkImageDir :: Text
    } deriving (Generic, Show)
instance ToJSON JDKBuildOutput
instance FromJSON JDKBuildOutput
