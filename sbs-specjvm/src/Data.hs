
module Data
    ( BenchMode(..)
    , BenchUnit(..)
    , BenchResults(..)
    , SpecJVMResults(..)
    ) where

import Prelude
    ( Float, Int, Show
    )
import Data.Text (Text)
import Data.Vector (Vector)

data BenchMode =
      Throughput
    | AverageTime
    | SampleTime
    | SingleShotTime
    deriving (Show)

data BenchUnit =
      OpsMin
    | OpsSec
    | OpsMs
    deriving (Show)

data BenchResults = BenchResults
    { name :: Text
    , mode :: BenchMode
    , count :: Int
    , score :: Float
    , error :: Float
    , units :: BenchUnit
    } deriving (Show)

data SpecJVMResults = SpecJVMResults
    { totalTimeSeconds :: Int
    , benchmarks :: Vector BenchResults
    } deriving (Show)
