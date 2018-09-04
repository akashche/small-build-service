
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
