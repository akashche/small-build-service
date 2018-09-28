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
    , extractSummary
    , diffTwoResults
    ) where

import Prelude ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Tier1
import SBS.Common.Utils

import Data

resolvePaths :: TaskContext -> Tier1Config -> Paths
resolvePaths ctx cf = Paths wd bd ep op mop sp qp
    where
        appd = appDir ctx
        wd = prependIfRelative appd (workDir (cf :: Tier1Config))
        bd = wd <> (buildDir (cf :: Tier1Config))
        ep = prependIfRelative appd (makePath cf)
        op = wd <> "tier1.log"
        mop = prependIfRelative appd (mockOutputPath (cf :: Tier1Config))
        sp = wd <> "tier1-summary.log"
        qp = (prependIfRelative appd (queriesDir ctx)) <> "queries-tier1.sql"

extractSummary :: Text -> Text -> IO ()
extractSummary _outputPath _destPath = do
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
