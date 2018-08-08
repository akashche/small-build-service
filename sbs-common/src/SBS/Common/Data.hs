
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Data
    ( DyLoadArgs(..)
    , Empty(..)
    ) where

import Prelude ()
import qualified Data.Aeson as Aeson

import SBS.Common.Prelude

data Empty = Empty
    deriving (Data, Generic, Show, Typeable)
instance ToJSON Empty where
    toJSON _ = Aeson.object []
instance FromJSON Empty where
    parseJSON _ = return Empty

data DyLoadArgs = DyLoadArgs
    { name :: Text
    } deriving (Typeable, Data, Generic, Show)
instance ToJSON DyLoadArgs

