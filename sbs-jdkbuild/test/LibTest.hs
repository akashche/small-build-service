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

import SBS.Common.JDKBuild
import Data
import Lib

testPaths :: Test
testPaths = TestLabel "testPaths" $ TestCase $ do
    let cf = mockConfig { workDir = "work" } :: JDKBuildConfig
    let paths = resolvePaths (mockCtx "/appdir") cf
    assertEqual "workDir" "/appdir/work" $ workDir (paths :: Paths)
    assertEqual "sourceDir" "/appdir/jdk" $ sourceDir (paths :: Paths)
    assertEqual "buildDir" "/appdir/jdk" $ buildDir (paths :: Paths)
    assertEqual "bootJdkDir" "/appdir/bootjdk" $ bootJdkDir (paths :: Paths)
    assertEqual "jtregDir" "/appdir/jtreg" $ jtregDir (paths :: Paths)
    assertEqual "hgPath" "/usr/bin/hg" $ hgPath (paths :: Paths)
    assertEqual "bashPath" "/bin/bash" $ bashPath (paths :: Paths)
    assertEqual "makePath" "/usr/bin/make" $ makePath (paths :: Paths)
    assertEqual "confOutPath" "/appdir/work/conf.log" $ confOutPath (paths :: Paths)
    assertEqual "makeOutPath" "/appdir/work/make.log" $ makeOutPath (paths :: Paths)
    assertEqual "mockOutputDir" "/appdir/mock" $ mockOutputDir (paths :: Paths)
    assertEqual "queriesPath" "/appdir/queries/queries-jdkbuild.sql" $ queriesPath (paths :: Paths)
    return ()

libTest :: Test
libTest = TestLabel "LibTest" (TestList
    [ testPaths
    ])
