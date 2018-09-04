
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.JCStress
    ( MyObjOut(..)
    ) where

import Prelude ()

import SBS.Common.Prelude

data MyObjOut = MyObjOut
    { baz :: Text
    , baa :: Int
    } deriving (Generic, Show, Typeable)
instance ToJSON MyObjOut
instance FromJSON MyObjOut
