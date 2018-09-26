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
    , spawnTestsAndWait
    , extractSummary
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Tier1
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

resolvePaths :: TaskContext -> Tier1Config -> Paths
resolvePaths ctx cf = Paths wd ep op mop sp qp
    where
        appd = appDir ctx
        wd = prependIfRelative appd (workDir (cf :: Tier1Config))
        ep = prependIfRelative appd (makePath cf)
        op = wd <> "tier1.log"
        mop = prependIfRelative appd (mockOutputPath (cf :: Tier1Config))
        sp = wd <> "tier1-summary.log"
        qp = (prependIfRelative appd (queriesDir ctx)) <> "queries-tier1.sql"

spawnTestsAndWait :: Paths -> Vector Text -> IO ()
spawnTestsAndWait paths args = do
    _code <- spawnProcess SpawnedProcessArgs
        { workDir = workDir (paths :: Paths)
        , executable = execPath paths
        , execArgs = args
        , outputFile = outputPath (paths :: Paths)
        , awaitExit = True
        }
    -- todo: add less strict check
    -- checkSpawnSuccess "tier1" code log
    return ()

extractSummary :: Text -> Text -> IO ()
extractSummary _outputPath _destPath = do
    return ()
