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

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.SpecJVM
-- import SBS.Common.Utils

import Data
import Lib
import Parser

main :: IO ()
main = do
    -- parser
    res <- parseResults "test/specjvm.log"
    unless (8703 == totalTimeSeconds res) (error "Time fail")
    _summ <- parseSummary "test/specjvm.log"

    -- diff
    res1 <- parseResults "test/specjvm_alt.log"
    let diff = diffResults res res1
    unless (100 == relativeTotalTime diff) (error "Diff time fail")
    unless (13 == Vector.length (benchmarks (diff :: ResultsDiff))) (error "Diff benches fail")

    -- diff format
    let _fd = formatResultsDiff diff

    -- paths
    let ctx = TaskContext
            { taskId = 42
            , dbConnection = DBConnection 43 44
            , appDir = "/foo"
            , queriesDir = "queries"
            , destBaseDir = ""
            , destDir = ""
            }
    let cf = SpecJVMConfig
            { enabled = True
            , workDir = "bar"
            , mockOutput = "mock.log"
            , outputFile = "specjvm.log"
            , summaryFile = "specjvm-summary.txt"
            , jdkDir = "jdk"
            , specjvmJarPath = "specjvm.jar"
            , ncNotePath = "ncnote.txt"
            , xmxMemoryLimitMB = 42
            , threadsCount = 43
            , excludedBenchmarks = fromList []
            }
    let paths = resolvePaths ctx cf
    when ("/foo/bar" /= workDir (paths :: Paths)) (error "Paths workDir fail")
    when ("/foo/jdk/bin/java" /= execPath (paths :: Paths)) (error "Paths execPath fail")
    when ("/foo/specjvm.jar" /= specjvmJarPath (paths :: Paths)) (error "Paths specjvmJarPath fail")
    when ("/foo/bar/specjvm.log" /= outputPath (paths :: Paths)) (error "Paths outputPath fail")
    when ("/foo/bar/specjvm-summary.txt" /= summaryPath (paths :: Paths)) (error "Paths summaryPath fail")
    when ("/foo/mock.log" /= mockOutputPath (paths :: Paths)) (error "Paths mockOutputPath fail")
    when ("/foo/queries/queries-specjvm.sql" /= queriesPath (paths :: Paths)) (error "Paths queriesPath fail")

    putStrLn "Tests Passed."
    return ()
