
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Data
    ( JCStressResults(..)
    , JCStressResultsDiff(..)
    ) where

import Prelude ()

import SBS.Common.Prelude

data JCStressResults = JCStressResults
    { passedCount :: Int
    , interesting :: Vector Text
    , failed :: Vector Text
    , error :: Vector Text
    } deriving (Show)

data JCStressResultsDiff = JCStressResultsDiff
    { passedDiff :: Int
    , interestingDiff :: Int
    , failedDiff :: Int
    , errorDiff :: Int
    } deriving (Show)
