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
    , TestSuite(..)
    , Results
    , TestSuiteDiff(..)
    , ResultsDiff
    ) where

import Prelude ()

import SBS.Common.Prelude

data Paths = Paths
    { workDir :: Text
    , buildDir :: Text
    , execPath :: Text
    , outputPath :: Text
    , mockOutputPath :: Text
    , summaryPath :: Text
    , queriesPath :: Text
    } deriving Show

data TestSuite = TestSuite
    { name :: Text
    , pass :: Int
    , fail :: Int
    , errored :: Int
    } deriving (Generic, Show)
instance ToJSON TestSuite
instance FromJSON TestSuite

type Results = Vector TestSuite

data TestSuiteDiff = TestSuiteDiff
    { name :: Text
    , notPassedDiff :: Maybe Int
    } deriving (Show)

type ResultsDiff = Vector TestSuiteDiff
