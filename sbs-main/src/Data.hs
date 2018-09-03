
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Data
    ( Config(..)
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.SpecJVM

data Config = Config
    { dbFilePath :: Text
    , jdkImageDir :: Text
    , specjvmConfig :: SpecJVMConfig
    } deriving (Typeable, Data, Generic, Show)
instance FromJSON Config
instance ToJSON Config


