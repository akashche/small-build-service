
{-# LANGUAGE DuplicateRecordFields #-}

module Data
    ( BenchMode(..)
    , BenchUnit(..)
    , BenchResult(..)
    , SpecJVMResults(..)
    ) where

import Prelude
    ( Float, Int, Show
    )
import Data.Text (Text)
import Data.Vector (Vector)

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
