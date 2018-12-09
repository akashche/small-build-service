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
import VtUtils.Prelude

import SBS.Common.Parsec

import Data

summaryHeader :: Text
summaryHeader = "TEST                                              TOTAL  PASS  FAIL ERROR   "

summaryFooter :: Text
summaryFooter = "=============================="

integer :: Parser Int
integer = do
    valStr <- many1 digit
    let val = read valStr :: Int
    parsecWhitespace
    return val

testSuite :: Parser TestSuite
testSuite = do
    optional (string ">>")
    parsecWhitespace
    _ <- string "jtreg:test/"
    nameStr <- many1 alphaNum
    let nameText = pack nameStr
    parsecSkipManyTill ":"
    _ <- string ":tier1"
    parsecWhitespace
    totalNum <- integer
    passNum <- integer
    failNum <- integer
    errorNum <- integer
    when (totalNum /= passNum + failNum + errorNum) ((error . unpack)
        (  "Wrong number of tests parsed,"
        <> " expected: [" <> (textShow totalNum) <> "]"
        <> " actual: [" <> (textShow (passNum + failNum + errorNum)) <> "]"
        ))
    parsecWhitespace
    optional (string "<<")
    parsecWhitespace
    return (TestSuite nameText passNum failNum errorNum)

results :: Parser Results
results = do
    _ <- parsecLinePrefix summaryHeader
    parsecWhitespace
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
    _ <- parsecLinePrefix summaryHeader
    parsecWhitespace
    st <- manyTill anyChar (string (unpack summaryFooter))
    let head = "   " <> summaryHeader <> "\n"
    return (head <> (pack st))

parseResults :: Text -> IO Results
parseResults path = do
    res <- parsecParseFile results path
    return res

parseSummary :: Text -> IO Text
parseSummary path = do
    res <- parsecParseFile summary path
    return res
