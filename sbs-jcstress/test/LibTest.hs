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

module LibTest (libTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude

import SBS.Common.Data
import SBS.Common.JCStress
import Data
import Lib

rc1 :: ResultsCount
rc1 = ResultsCount 1 2 3 4

rc2 :: ResultsCount
rc2 = ResultsCount 2 4 1 3

diff :: ResultsDiff
diff = diffResultsCount rc1 rc2

ctx :: TaskContext
ctx = TaskContext
    { taskId = 42
    , dbConnection = DBConnection 43 44
    , appDir = "/foo"
    , queriesDir = "queries"
    , destBaseDir = ""
    , destDir = ""
    }

cf :: JCStressConfig
cf = JCStressConfig
    { enabled = True
    , workDir = "bar"
    , mockOutput = "mock.log"
    , outputFile = "jcstress.log"
    , summaryFile = "jcstress-summary.txt"
    , jdkDir = "jdk"
    , jcstressJarPath = "jcstress.jar"
    , xmxMemoryLimitMB = 42
    , mode = ""
    }

testDiff :: Test
testDiff = TestLabel "testDiff" $ TestCase $ do
    assertEqual "passed" (-1) $ passedDiff diff
    assertEqual "failed" 2 $ failedDiff diff
    return ()

testDiffFormat :: Test
testDiffFormat = TestLabel "testDiffFormat" $ TestCase $ do
    assertEqual "format" "passed: -1; interesting: -2; failed: 2; error: 1" $
        formatResultsDiff diff
    return ()

testSummary :: Test
testSummary = TestLabel "testSummary" $ TestCase $ do
    let res = Results 42 (fromList []) (fromList []) (fromList [])
    assertEqual "summary" "passed: 42\ninteresting: 0\nfailed: 0\nerror: 0" $
        formatSummary res
    return ()

testPaths :: Test
testPaths = TestLabel "testPaths" $ TestCase $ do
    let paths = resolvePaths ctx cf
    assertEqual "workDir" "/foo/bar" $ workDir (paths :: Paths)
    assertEqual "execPath" "/foo/jdk/bin/java" $ execPath (paths :: Paths)
    assertEqual "jcstressJarPath" "/foo/jcstress.jar" $ jcstressJarPath (paths :: Paths)
    assertEqual "outputPath" "/foo/bar/jcstress.log" $ outputPath (paths :: Paths)
    assertEqual "summaryPath" "/foo/bar/jcstress-summary.txt" $ summaryPath (paths :: Paths)
    assertEqual "mockOutputPath" "/foo/mock.log" $ mockOutputPath (paths :: Paths)
    assertEqual "queriesPath" "/foo/queries/queries-jcstress.sql" $ queriesPath (paths :: Paths)
    return ()

libTest :: Test
libTest = TestLabel "LibTest" (TestList
    [ testDiff
    , testDiffFormat
    , testSummary
    , testPaths
    ])
