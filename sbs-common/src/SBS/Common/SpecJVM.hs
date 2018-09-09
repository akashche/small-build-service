
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.SpecJVM
    ( SpecJVMConfig(..)
    , SpecJVMInput(..)
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data

data SpecJVMConfig = SpecJVMConfig
    { xmxMemoryLimitMB :: Int
    , threadsCount :: Int
    , excludedBenchmarks :: Vector Text
    } deriving (Generic, Show)
instance FromJSON SpecJVMConfig
instance ToJSON SpecJVMConfig

data SpecJVMInput = SpecJVMInput
    { jdkImageDir :: Text
    , dbConfig :: DBConfig
    , config :: SpecJVMConfig
    } deriving (Generic, Show)
instance FromJSON SpecJVMInput
instance ToJSON SpecJVMInput
