
module Data
    ( ) where

import Prelude
    ( Float, Int, Show
    )

import Data.Text (Text)
import Data.Vector (Vector)

data BenchResults = BenchResults
    { name :: Text
    , mode :: Text
    , count :: Int
    , score :: Float
    , error :: Float
    , units :: Text
    } deriving (Show)

data JCStressResults = JCStressResults
    { totalTimeSeconds :: Int
    , benchmarks :: Vector BenchResults
    } deriving (Show)
