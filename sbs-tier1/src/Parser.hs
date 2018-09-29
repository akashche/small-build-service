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

module Parser
    ( parseResults
    , parseSummary
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Parsec
import SBS.Common.Utils

import Data

summaryHeader :: Text
summaryHeader = "TEST                                              TOTAL  PASS  FAIL ERROR   "

summaryFooter :: Text
summaryFooter = "=============================="

testSuite :: Parser TestSuite
testSuite = do
    optional (string ">>")
    whitespace
    skipOne (string "jtreg:test/")
    nameStr <- many1 alphaNum
    let nameText = pack nameStr
    skipManyTill ":"
    skipOne (string ":tier1")
    whitespace
    totalNum <- integer
    passNum <- integer
    failNum <- integer
    errorNum <- integer
    when (totalNum /= passNum + failNum + errorNum) ((error . unpack)
        (  "Wrong number of tests parsed,"
        <> " expected: [" <> (showText totalNum) <> "]"
        <> " actual: [" <> (showText (passNum + failNum + errorNum)) <> "]"
        ))
    whitespace
    optional (string "<<")
    whitespace
    return (TestSuite nameText passNum failNum errorNum)

results :: Parser Results
results = do
    skipLinesTill summaryHeader
    list <- scan
    return (fromList list)
    where
        scan = recur <|> (return [])
        recur = do
            x <- testSuite
            xs <- scan
            return (x:xs)

summary :: Parser Text
summary = do
    skipLinesTill summaryHeader
    st <- manyTill anyChar (string (unpack summaryFooter))
    let head = "   " <> summaryHeader <> "\n"
    return (head <> (pack st))

parseResults :: Text -> IO Results
parseResults path = do
    res <- parseFile results path
    return res

parseSummary :: Text -> IO Text
parseSummary path = do
    res <- parseFile summary path
    return res
