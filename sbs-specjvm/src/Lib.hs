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
    , diffResults
    , copyNcNote
    , formatResultsDiff
    ) where

import Prelude ()
import VtUtils.Prelude
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text
import qualified System.Directory as Directory

import SBS.Common.Data
import SBS.Common.SpecJVM

import Data

resolvePaths :: TaskContext -> SpecJVMConfig -> Paths
resolvePaths ctx cf = Paths
    { workDir = wd
    , execPath = pathConcat (pathPrepend appd (jdkDir cf)) "bin/java"
    , specjvmJarPath = pathPrepend appd (specjvmJarPath (cf :: SpecJVMConfig))
    , outputPath = pathConcat wd (outputFile (cf :: SpecJVMConfig))
    , summaryPath = pathConcat wd (summaryFile (cf :: SpecJVMConfig))
    , mockOutputPath = pathPrepend appd (mockOutput (cf :: SpecJVMConfig))
    , queriesPath = pathConcat (pathPrepend appd qdir) "queries-specjvm.sql"
    }
    where
        appd = appDir (ctx :: TaskContext)
        qdir = queriesDir (ctx :: TaskContext)
        wd = pathPrepend appd (workDir (cf :: SpecJVMConfig))

mockPaths :: Paths
mockPaths = Paths
    { workDir = "."
    , execPath = "jdk/bin/java"
    , specjvmJarPath = "jmh-specjvm2016.jar"
    , outputPath = "specjvm.log"
    , summaryPath = "specjvm-summary.txt"
    , mockOutputPath = ""
    , queriesPath = ""
    }

mockConfig :: SpecJVMConfig
mockConfig = SpecJVMConfig
    { enabled = True
    , workDir = "."
    , mockOutput = ""
    , outputFile = "specjvm.log"
    , summaryFile = "specjvm-summary.txt"
    , jdkDir = ""
    , specjvmJarPath = ""
    , ncNotePath = ""
    , xmxMemoryLimitMB = 1024
    , threadsCount = 2
    , excludedBenchmarks = fromList [
        "Compiler.compiler",
        "CryptoAes.test",
        "Derby.test",
        "ScimarkFFT.large",
        "ScimarkLU.large",
        "ScimarkSOR.large",
        "ScimarkSparse.large",
        "ScimarkSparse.small"
    ]}

diffResults :: Results -> Results -> ResultsDiff
diffResults baseline res =
    ResultsDiff relTime benches
    where
        nm el = name (el :: BenchResult)
        relTime = div ((totalTimeSeconds res) * 100) (totalTimeSeconds baseline)
        bsBase = benchmarks (baseline :: Results)
        bsRes = benchmarks (res :: Results )
        pairFolder el li = ((nm el, el) : li)
        pairs = foldr' pairFolder [] bsRes
        hmap = HashMap.fromList pairs
        diffScore el1 el2 = div ((score el1) * 100) (score el2)
        diff el = fmap (diffScore el) (HashMap.lookup (nm el) hmap)
        folder el li = (BenchDiff (nm el) (diff el)  : li)
        benches = fromList (foldr' folder [] bsBase)

copyNcNote :: SpecJVMConfig -> Text -> IO ()
copyNcNote cf appd = Directory.copyFile (unpack from) (unpack to)
    where
        from = pathPrepend appd (ncNotePath cf)
        wd = pathPrepend appd (workDir (cf :: SpecJVMConfig))
        to = pathConcat wd "nc_note.txt"

formatPercent :: Int -> Text
formatPercent num =
    if num < 100
        then "0." <> ntx
        else (Text.take fract ntx) <> "." <> (Text.drop fract ntx)
    where
        ntx = textShow num
        fract = (Text.length ntx) - 2

formatResultsDiff :: ResultsDiff -> Text
formatResultsDiff rd =
    ifoldl' folder "" benches
    where
        benches = benchmarks (rd :: ResultsDiff)
        showDiff el = case (relativeScore (el :: BenchDiff)) of
            Just num -> formatPercent num
            Nothing -> "N/A"
        folder ac idx el =
               ac
            <> (if idx > 0 then "; " else "")
            <> (name (el :: BenchDiff))
            <> ": "
            <> showDiff (el)
