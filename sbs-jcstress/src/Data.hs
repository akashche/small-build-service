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
    ( Paths(..)
    , Results(..)
    , ResultsDiff(..)
    , ResultsCount(..)
    ) where

import Prelude ()
import VtUtils.Prelude

data Paths = Paths
    { workDir :: Text
    , execPath :: Text
    , jcstressJarPath :: Text
    , outputPath :: Text
    , summaryPath :: Text
    , mockOutputPath :: Text
    , queriesPath :: Text
    } deriving (Show)

data Results = Results
    { passedCount :: Int
    , interesting :: Vector Text
    , failed :: Vector Text
    , errored :: Vector Text
    } deriving (Show)

data ResultsCount = ResultsCount
    { passedCount :: Int
    , interestingCount :: Int
    , failedCount :: Int
    , errorCount :: Int
    } deriving (Generic, Show)
instance ToJSON ResultsCount
instance FromJSON ResultsCount

data ResultsDiff = ResultsDiff
    { passedDiff :: Int
    , interestingDiff :: Int
    , failedDiff :: Int
    , errorDiff :: Int
    } deriving (Show)

