
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

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Queries

data JCStressConfig = JCStressConfig
    { enabled :: Bool
    , mockOutput :: Text
    , baselineOutput :: Text
    , jcstressJarPath :: Text
    , xmxMemoryLimitMB :: Int
    , mode :: Text
    } deriving (Generic, Show)
instance ToJSON JCStressConfig
instance FromJSON JCStressConfig

data JCStressInput = JCStressInput
    { taskId :: Int64
    , dbConnection :: DBConnection
    , jdkImageDir :: Text
    , queries :: Queries
    , jcstressConfig :: JCStressConfig
    } deriving (Generic, Show)
instance ToJSON JCStressInput
instance FromJSON JCStressInput
