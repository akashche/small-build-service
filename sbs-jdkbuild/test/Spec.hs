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

import SBS.Common.Prelude
import SBS.Common.JDKBuild

import Data
import Lib
import Parser

main :: IO ()
main = do
    -- parser
    cd <- parseConfOutput "test/conf.log"
    unless ("/home/server/tmp/jdkbuild/build/" == confDirectory cd) (error "Conf parse fail")
    md <- parseMakeOutput "test/make.log"
    unless ("images/jdk/" == imageDirRelative md) (error "Make parse fail")
    mdOld <- parseMakeOutput "test/make_old.log"
    unless ("images/jdk/" == imageDirRelative mdOld) (error "Make parse fail")

    -- paths
    let cf = mockConfig { workDir = "work/" } :: JDKBuildConfig
    let paths = resolvePaths (mockCtx "appdir/") cf
    unless ("appdir/work/" == workDir (paths :: Paths)) (error "Paths workDir fail")
    unless ("appdir/jdk/" == sourceDir (paths :: Paths)) (error "Paths sourceDir fail")
    unless ("appdir/jdk/" == buildDir (paths :: Paths)) (error "Paths buildDir fail")
    unless ("appdir/bootjdk/" == bootJdkDir (paths :: Paths)) (error "Paths bootJdkDir fail")
    unless ("appdir/jtreg/" == jtregDir (paths :: Paths)) (error "Paths jtregDir fail")
    unless ("/usr/bin/hg" == hgPath (paths :: Paths)) (error "Paths hgPath fail")
    unless ("/bin/bash" == bashPath (paths :: Paths)) (error "Paths bashPath fail")
    unless ("/usr/bin/make" == makePath (paths :: Paths)) (error "Paths makePath fail")
    unless ("appdir/work/conf.log" == confOutPath (paths :: Paths)) (error "Paths confOutPath fail")
    unless ("appdir/work/make.log" == makeOutPath (paths :: Paths)) (error "Paths makeOutPath fail")
    unless ("appdir/mock/" == mockOutputDir (paths :: Paths)) (error "Paths mockOutputDir fail")
    unless ("appdir/queries/queries-jdkbuild.sql" == queriesPath (paths :: Paths)) (error "Paths queriesPath fail")

    putStrLn "Tests Passed."
    return ()

