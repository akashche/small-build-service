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
import VtUtils.Prelude

import SBS.Common.Data
import SBS.Common.JCStress

import Data

resolvePaths :: TaskContext -> JCStressConfig -> Paths
resolvePaths ctx cf = Paths
    { workDir = wd
    , execPath = pathConcat (pathPrepend appd (jdkDir cf)) "bin/java"
    , jcstressJarPath = pathPrepend appd (jcstressJarPath (cf :: JCStressConfig))
    , outputPath = pathConcat wd (outputFile (cf :: JCStressConfig))
    , summaryPath = pathConcat wd (summaryFile (cf :: JCStressConfig))
    , mockOutputPath = pathPrepend appd (mockOutput (cf :: JCStressConfig))
    , queriesPath = pathConcat (pathPrepend appd qdir) "queries-jcstress.sql"
    }
    where
        appd = appDir (ctx :: TaskContext)
        qdir = queriesDir (ctx :: TaskContext)
        wd = pathPrepend appd (workDir (cf :: JCStressConfig))

mockPaths :: Paths
mockPaths = Paths
    { workDir = "."
    , execPath = "jdk/bin/java"
    , jcstressJarPath = "jcstress.jar"
    , outputPath = "jcstress.log"
    , summaryPath = "jcstress-summary.txt"
    , mockOutputPath = ""
    , queriesPath = ""
    }

mockConfig :: JCStressConfig
mockConfig = JCStressConfig
    { enabled = True
    , workDir = ""
    , mockOutput = ""
    , outputFile = "jcstress.log"
    , summaryFile = "jcstress-summary.txt"
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
       "passed: " <> (textShow (passedCount (res :: Results))) <> "\n"
    <> "interesting: " <> (textShow (length (interesting res))) <> "\n"
    <> "failed: " <> (textShow (length (failed res))) <> "\n"
    <> "error: " <> (textShow (length (errored res)))

formatResultsDiff :: ResultsDiff -> Text
formatResultsDiff rd =
       "passed: " <> (textShow (passedDiff rd)) <> ";"
    <> " interesting: " <> (textShow (interestingDiff rd)) <> ";"
    <> " failed: " <> (textShow (failedDiff rd)) <> ";"
    <> " error: " <> (textShow (errorDiff rd))

totalFailOrError :: Results -> Int
totalFailOrError res =
    length (failed res) + length (errored res)
