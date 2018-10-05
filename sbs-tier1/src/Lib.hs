--
-- Copyright 2018, akashche at redhat.com
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
-- http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Lib
    ( Paths(..)
    , resolvePaths
    , resolveQueriesPath
    , extractSummary
    , diffTwoResults
    , formatResultsDiff
    , totalNotPassed
    ) where

import Prelude ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Tier1
import SBS.Common.Utils

import Data
import Parser

resolvePaths :: TaskContext -> Tier1Config -> Paths
resolvePaths ctx cf = Paths
    { workDir = wd
    , buildDir = prependIfRelative appd (buildDir (cf :: Tier1Config))
    , execPath = prependIfRelative appd (makePath cf)
    , outputPath = wd <> "tier1.log"
    , mockOutputPath = prependIfRelative appd (mockOutputPath (cf :: Tier1Config))
    , summaryPath = wd <> "tier1-summary.log"
    , queriesPath = (prependIfRelative appd qdir) <> "queries-tier1.sql"
    }
    where
        appd = appDir (ctx :: TaskContext)
        qdir = queriesDir (ctx :: TaskContext)
        wd = prependIfRelative appd (workDir (cf :: Tier1Config))

resolveQueriesPath :: DiffRequest -> Text
resolveQueriesPath req =
    (prependIfRelative appd qdir) <> "queries-tier1.sql"
    where
        appd = appDir (req :: DiffRequest)
        qdir = queriesDir (req :: DiffRequest)

extractSummary :: Text -> Text -> IO ()
extractSummary outp destp = do
    sm <- parseSummary outp
    writeFile (unpack destp) sm
    return ()

diffTwoResults :: Results -> Results -> ResultsDiff
diffTwoResults res1 res2 =
    fromList (Vector.foldr' folder [] res1)
    where
        nm el = name (el :: TestSuite)
        pairFolder el li = ((nm el, el) : li)
        pairs = Vector.foldr' pairFolder [] res2
        hmap = HashMap.fromList pairs
        nonPassed el = (fail el) + (errored el)
        diffNonPassed el1 el2 = (nonPassed el1) - (nonPassed el2)
        diff el1 = fmap (diffNonPassed el1) (HashMap.lookup (nm el1) hmap)
        folder el1 li = (TestSuiteDiff (nm el1) (diff el1)  : li)

formatResultsDiff :: ResultsDiff -> Text
formatResultsDiff rd =
    Vector.ifoldl' folder "" rd
    where
        showDiff el = case (notPassedDiff (el :: TestSuiteDiff)) of
            Just num -> showText num
            Nothing -> "="
        folder ac idx el =
               ac
            <> (if idx > 0 then "; " else "")
            <> (name (el :: TestSuiteDiff))
            <> ": "
            <> showDiff (el)

totalNotPassed :: Results -> Int
totalNotPassed res =
    Vector.foldl' folder 0 res
    where
        folder np el = np + (fail el) + (errored el)
