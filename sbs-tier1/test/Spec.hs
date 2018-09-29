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
import qualified Data.Vector.Mutable as MVector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Tier1

import Data
import Lib
import Parser

main :: IO ()
main = do

    -- parse results

    res <- parseResults "test/tier1.log"
    when (5 /= Vector.length res) (error "Parse length fail")
    when ("hotspot" /= name ((res ! 0) :: TestSuite)) (error "Parse fail")
    when (3934 /= pass ((res ! 2) :: TestSuite)) (error "Parse fail")
    when (12 /= fail ((res ! 1) :: TestSuite)) (error "Parse fail")

    -- parse summary
    summary <- parseSummary "test/tier1.log"
    putStrLn summary

    -- diff

    let resNew = runST (do
            let short = Vector.take ((Vector.length res) - 1) res
            let modifier el = el { fail = (fail el) + 42 }
            mv <- Vector.thaw short
            MVector.modify mv modifier 0
            frozen <- Vector.freeze mv
            return frozen )
    let diff = diffTwoResults res resNew
    when (5 /= Vector.length diff) (error "Diff length fail")
    when ((-42) /= fromJust (notPassedDiff (diff ! 0))) (error "Diff fail")
    when (isJust (notPassedDiff (diff ! 4))) (error "Diff fail")

    -- paths
    let ctx = TaskContext
            { taskId = 42
            , dbConnection = DBConnection 43 44
            , appDir = "/foo/"
            , queriesDir = "queries/"
            }
    let cf = Tier1Config
            { enabled = True
            , workDir = "bar/"
            , mockOutputPath = ""
            , buildDir = "build/"
            , makePath = "/usr/bin/make"
            , target = "run-test-tier1"
            }
    let paths = resolvePaths ctx cf
    when ("/foo/bar/" /= workDir (paths :: Paths)) (error "Paths workDir fail")
    when ("/foo/bar/build/" /= buildDir (paths :: Paths)) (error "Paths buildDir fail")
    when ("/usr/bin/make" /= execPath (paths :: Paths)) (error "Paths execPath fail")
    when ("/foo/bar/tier1.log" /= outputPath (paths :: Paths)) (error "Paths outputPath fail")
    when ("/foo/bar/tier1-summary.log" /= summaryPath (paths :: Paths)) (error "Paths summaryPath fail")
    when ("/foo/queries/queries-tier1.sql" /= queriesPath (paths :: Paths)) (error "Paths queriesPath fail")

    putStrLn "Tests Passed."
    return ()

