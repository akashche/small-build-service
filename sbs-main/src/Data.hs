
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Data
    ( Config(..)
    , CreateDbConfig(..)
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.JCStress
import SBS.Common.SpecJVM

data CreateDbConfig = CreateDbConfig
    { enabled :: Bool
    , ddlPath :: Text
    } deriving (Generic, Show)
instance FromJSON CreateDbConfig
instance ToJSON CreateDbConfig

data Config = Config
    { dbFilePath :: Text
    , queriesDir :: Text
    , createDb :: CreateDbConfig
    , jdkImageDir :: Text
    , jcstress :: JCStressConfig
    , specjvm :: SpecJVMConfig
    } deriving (Generic, Show)
instance FromJSON Config
instance ToJSON Config
