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
    , BenchMode(..)
    , BenchUnit(..)
    , BenchResult(..)
    , Results(..)
    , BenchDiff(..)
    , ResultsDiff(..)
    ) where

import Prelude ()
import VtUtils.Prelude

data Paths = Paths
    { workDir :: Text
    , execPath :: Text
    , specjvmJarPath :: Text
    , outputPath :: Text
    , summaryPath :: Text
    , mockOutputPath :: Text
    , queriesPath :: Text
    } deriving (Show)

data BenchMode =
      All
    | AverageTime
    | SampleTime
    | SingleShotTime
    | Throughput
    deriving (Show, Read)

data BenchUnit =
      OpsMin
    | OpsSec
    | OpsMs
    deriving (Show, Read)

data BenchResult = BenchResult
    { name :: Text
    , mode :: BenchMode
    , count :: Int
    , score :: Int
    , errored :: Int
    , units :: BenchUnit
    } deriving (Show)

data Results = Results
    { totalTimeSeconds :: Int
    , benchmarks :: Vector BenchResult
    } deriving (Show)

data BenchDiff = BenchDiff
    { name :: Text
    , relativeScore :: Maybe Int
    } deriving (Show)

data ResultsDiff = ResultsDiff
    { relativeTotalTime :: Int
    , benchmarks :: Vector BenchDiff
    } deriving (Show)
