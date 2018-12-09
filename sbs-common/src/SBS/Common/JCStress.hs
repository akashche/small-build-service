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

module SBS.Common.JCStress
    ( JCStressConfig(..)
    , JCStressInput(..)
    ) where

import Prelude ()
import VtUtils.Prelude

import SBS.Common.Data

data JCStressConfig = JCStressConfig
    { enabled :: Bool
    , workDir :: Text
    , mockOutput :: Text
    , outputFile :: Text
    , summaryFile :: Text
    , jdkDir :: Text
    , jcstressJarPath :: Text
    , xmxMemoryLimitMB :: Int
    , mode :: Text
    } deriving (Generic, Show)
instance ToJSON JCStressConfig
instance FromJSON JCStressConfig

data JCStressInput = JCStressInput
    { taskCtx :: TaskContext
    , jcstressConfig :: JCStressConfig
    } deriving (Generic, Show)
instance ToJSON JCStressInput
instance FromJSON JCStressInput
