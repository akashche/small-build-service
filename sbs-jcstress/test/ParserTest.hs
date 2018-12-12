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

testParseResults :: Test
testParseResults = TestLabel "testParseResults" $ TestCase $ do
    res <- parseResults "test/jcstress_abridged.log"
    assertEqual "passed" 4875 $ passedCount (res :: Results)
    assertEqual "interesting" 5 $ length (interesting res)
    assertEqual "failed" 0 $ length (failed res)
    assertEqual "errored" 0 $ length (errored res)
    return ()

parserTest :: Test
parserTest = TestLabel "ParserTest" (TestList
    [ testParseResults
    ])
