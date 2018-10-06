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
    ( resolvePaths
    , mockPaths
    , mockConfig
    , diffResultsCount
    , formatSummary
    , formatResultsDiff
    , totalFailOrError
    ) where

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JCStress
import SBS.Common.Utils

import Data

resolvePaths :: TaskContext -> JCStressConfig -> Paths
resolvePaths ctx cf = Paths
    { workDir = wd
    , execPath = (prependIfRelative appd (jdkDir cf)) <> "bin/java"
    , jcstressJarPath = prependIfRelative appd (jcstressJarPath (cf :: JCStressConfig))
    , outputPath = wd <> "jcstress.log"
    , summaryPath = wd <> "jcstress-summary.log"
    , mockOutputPath = prependIfRelative appd (mockOutput (cf :: JCStressConfig))
    , queriesPath = (prependIfRelative appd qdir) <> "queries-jcstress.sql"
    }
    where
        appd = appDir (ctx :: TaskContext)
        qdir = queriesDir (ctx :: TaskContext)
        wd = prependIfRelative appd (workDir (cf :: JCStressConfig))

mockPaths :: Paths
mockPaths = Paths
    { workDir = "./"
    , execPath = "jdk/bin/java"
    , jcstressJarPath = "jcstress.jar"
    , outputPath = "jcstress.log"
    , summaryPath = "jcstress-summary.log"
    , mockOutputPath = ""
    , queriesPath = ""
    }

mockConfig :: JCStressConfig
mockConfig = JCStressConfig
    { enabled = True
    , workDir = ""
    , mockOutput = ""
    , jdkDir = ""
    , jcstressJarPath = ""
    , xmxMemoryLimitMB = 1024
    , mode = "quick"
    }

diffResultsCount :: ResultsCount -> ResultsCount -> ResultsDiff
diffResultsCount res1 res2 =
    ResultsDiff pd xd fd ed
    where
        spassed res = passedCount (res :: ResultsCount)
        pd = (spassed res1) - (spassed res2)
        xd = (interestingCount res1) - (interestingCount res2)
        fd = (failedCount res1) - (failedCount res2)
        ed = (errorCount res1) - (errorCount res2)

formatSummary :: Results -> Text
formatSummary res =
       "passed: " <> (showText (passedCount (res :: Results))) <> "\n"
    <> "interesting: " <> (showText (Vector.length (interesting res))) <> "\n"
    <> "failed: " <> (showText (Vector.length (failed res))) <> "\n"
    <> "error: " <> (showText (Vector.length (errored res)))

formatResultsDiff :: ResultsDiff -> Text
formatResultsDiff rd =
       "passed: " <> (showText (passedDiff rd)) <> ";"
    <> " interesting: " <> (showText (interestingDiff rd)) <> ";"
    <> " failed: " <> (showText (failedDiff rd)) <> ";"
    <> " error: " <> (showText (errorDiff rd))

totalFailOrError :: Results -> Int
totalFailOrError res =
    Vector.length (failed res) + Vector.length (errored res)
