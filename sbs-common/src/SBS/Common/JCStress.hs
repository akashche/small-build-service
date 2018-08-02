
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SBS.Common.JCStress
    ( MyObjOut(..)
    ) where

import Prelude
    ( Int, Show
    )
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Text (Text)
import Data.Vector (Vector)
import GHC.Generics (Generic)

data MyObjOut = MyObjOut
    { baz :: Text
    , baa :: Int
    } deriving (Data, Generic, Show, Typeable)
instance ToJSON MyObjOut
instance FromJSON MyObjOut
