
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Data
    ( JCStressResults(..)
    ) where

import Prelude ()

import SBS.Common.Prelude

data JCStressResults = JCStressResults
    { passedCount :: Int
    , interested :: Vector Text
    , failed :: Vector Text
    , errored :: Vector Text
    } deriving (Show)
