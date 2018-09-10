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
    ( BenchMode(..)
    , BenchUnit(..)
    , BenchResult(..)
    , SpecJVMResults(..)
    , BenchDiff(..)
    , SpecJVMResultsDiff(..)
    ) where

import Prelude ()

import SBS.Common.Prelude

data BenchMode =
      All
    | AverageTime
    | SampleTime
    | SingleShotTime
    | Throughput
    deriving (Show)

data BenchUnit =
      OpsMin
    | OpsSec
    | OpsMs
    deriving (Show)

data BenchResult = BenchResult
    { name :: Text
    , mode :: BenchMode
    , count :: Int
    , score :: Int
    , error :: Int
    , units :: BenchUnit
    } deriving (Show)

data SpecJVMResults = SpecJVMResults
    { totalTimeSeconds :: Int
    , benchmarks :: Vector BenchResult
    } deriving (Show)

data BenchDiff = BenchDiff
    { name :: Text
    , relativeScore :: Int
    } deriving (Show)

data SpecJVMResultsDiff = SpecJVMResultsDiff
    { relativeTotalTime :: Int
    , benchmarks :: Vector BenchDiff
    } deriving (Show)
