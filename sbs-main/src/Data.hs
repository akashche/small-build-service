
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
import SBS.Common.SpecJVM

data CreateDbConfig = CreateDbConfig
    { enabled :: Bool
    , ddlPath :: Text
    } deriving (Generic, Show, Typeable)
instance FromJSON CreateDbConfig
instance ToJSON CreateDbConfig

data Config = Config
    { dbFilePath :: Text
    , queriesPath :: Text
    , createDb :: CreateDbConfig
    , jdkImageDir :: Text
    , specjvm :: SpecJVMConfig
    } deriving (Generic, Show, Typeable)
instance FromJSON Config
instance ToJSON Config


