
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SBS.Common.Data
    ( DyLoadArgs(..)
    , Empty(..)
    ) where

import Prelude
    ( Int, Show
    , return
    )
import Data.Aeson (FromJSON, ToJSON, object, parseJSON, toJSON)
import Data.Data (Data, Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)

data Empty = Empty
    deriving (Data, Generic, Show, Typeable)
instance ToJSON Empty where
    toJSON _ = object []
instance FromJSON Empty where
    parseJSON _ = return Empty

data DyLoadArgs = DyLoadArgs
    { name :: Text
    } deriving (Typeable, Data, Generic, Show)
instance ToJSON DyLoadArgs

