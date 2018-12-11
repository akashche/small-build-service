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

module QueriesTest (queriesTest) where

import Test.HUnit
import Prelude ()
import VtUtils.Prelude
import qualified Data.HashMap.Strict as HashMap

import SBS.Common.Data
import SBS.Common.Queries

testLoad :: Test
testLoad = TestLabel "testLoad" $ TestCase $ do
    qrs <- queriesLoad "../resources/queries-main.sql"
    assertEqual "five" 5 $ HashMap.size qrs
    return ()

testResolvePath :: Test
testResolvePath = TestLabel "testResolvePath" $ TestCase $ do
    let req = DiffRequest
            { taskId1 = 42
            , taskId2 = 42
            , dbConnection = DBConnection 42 42
            , appDir = "/appdir"
            , queriesDir = "qdir"
            }
    assertEqual "path" "/appdir/qdir/queries-foo.sql" $ resolveQueriesPath req "foo"
    return ()

queriesTest :: Test
queriesTest = TestLabel "QueriesTest" (TestList
    [ testLoad
    , testResolvePath
    ])
