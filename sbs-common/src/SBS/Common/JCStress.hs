
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SBS.Common.JCStress
    ( MyObjOut
    , createMyObjOut
    ) where

import Prelude
    ( Int, Show
    )
import Data.Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Text (Text)
import GHC.Generics (Generic)

data MyObjOut = MyObjOut
    { baz :: Text
    , baa :: Int
    } deriving (Data, Generic, Show, Typeable)
instance ToJSON MyObjOut
instance FromJSON MyObjOut
createMyObjOut :: Text -> Int -> MyObjOut
createMyObjOut baz_ baa_ = MyObjOut
    { baz = baz_
    , baa = baa_
    }

