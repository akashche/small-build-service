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
import SBS.Common.JCStress

import Data
import Lib
import Parser

main :: IO ()
main = do
    -- parser
    res <- parseResults "test/jcstress_abridged.log"
    unless (4875 == passedCount (res :: Results)) (error "Parser passed fail")
    unless (5 == (Vector.length (interesting res))) (error "Parser interesting fail")
    unless (0 == (Vector.length (failed res))) (error "Parser failed fail")
    unless (0 == (Vector.length (errored res))) (error "Parser errored fail")

    -- diff
    let rc1 = ResultsCount 1 2 3 4
    let rc2 = ResultsCount 2 4 1 3
    let diff = diffResultsCount rc1 rc2
    unless (-1 == passedDiff diff) (error "Diff passed fail")
    unless (2 == failedDiff diff) (error "Diff failed fail")

    -- diff format
    unless ("passed: -1; interesting: -2; failed: 2; error: 1" == formatResultsDiff diff) (error "Diff format fail")

    -- summary
    unless ("passed: 4875\ninteresting: 5\nfailed: 0\nerror: 0" == (formatSummary res)) (error "Summary fail")

    -- paths
    let ctx = TaskContext
            { taskId = 42
            , dbConnection = DBConnection 43 44
            , appDir = "/foo/"
            , queriesDir = "queries/"
            }
    let cf = JCStressConfig
            { enabled = True
            , workDir = "bar/"
            , mockOutput = "mock.log"
            , jdkDir = "jdk/"
            , jcstressJarPath = "jcstress.jar"
            , xmxMemoryLimitMB = 42
            , mode = ""
            }
    let paths = resolvePaths ctx cf
    when ("/foo/bar/" /= workDir (paths :: Paths)) (error "Paths workDir fail")
    when ("/foo/jdk/bin/java" /= execPath (paths :: Paths)) (error "Paths execPath fail")
    when ("/foo/jcstress.jar" /= jcstressJarPath (paths :: Paths)) (error "Paths jcstressJarPath fail")
    when ("/foo/bar/jcstress.log" /= outputPath (paths :: Paths)) (error "Paths outputPath fail")
    when ("/foo/bar/jcstress-summary.log" /= summaryPath (paths :: Paths)) (error "Paths summaryPath fail")
    when ("/foo/mock.log" /= mockOutputPath (paths :: Paths)) (error "Paths mockOutputPath fail")
    when ("/foo/queries/queries-jcstress.sql" /= queriesPath (paths :: Paths)) (error "Paths queriesPath fail")

    putStrLn "Tests Passed."
    return ()
