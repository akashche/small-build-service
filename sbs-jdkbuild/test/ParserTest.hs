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

module ParserTest (parserTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude

import Data
import Parser

testParseConfOutput :: Test
testParseConfOutput = TestLabel "testParseConfOutput" $ TestCase $ do
    cd <- parseConfOutput "test/conf.log"
    assertEqual "conf dir" "/home/server/tmp/jdkbuild/build" $
        confDirectory cd
    return ()

testParseMakeOutput :: Test
testParseMakeOutput = TestLabel "testParseMakeOutput" $ TestCase $ do
    md <- parseMakeOutput "test/make.log"
    assertEqual "make dir" "images/jdk" $ imageDirRelative md
    mdOld <- parseMakeOutput "test/make_old.log"
    assertEqual "make dir old" "images/jdk" $ imageDirRelative mdOld
    return ()

parserTest :: Test
parserTest = TestLabel "ParserTest" (TestList
    [ testParseConfOutput
    , testParseMakeOutput
    ])
