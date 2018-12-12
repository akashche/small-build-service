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
    ) where

import Prelude ()
import VtUtils.Prelude

import SBS.Common.Parsec

import Data

oneTest :: Text -> Parser Text
oneTest prefix = do
    _ <- (parsecTry (string (unpack prefix)))
    parsecWhitespace
    res <- manyTill (alphaNum <|> char '.') (char '\n')
    parsecSkipManyTill "\n\n"
    parsecWhitespace
    optional $ do
        _ <- parsecTry (string "Messages:")
        parsecSkipManyTill "\n\n"
        parsecWhitespace
    return (pack res)

listOfTests :: Text -> Text -> Parser (Vector Text)
listOfTests header prefix = do
    _ <- parsecLinePrefix header
    parsecWhitespace
    parsecSkipLines 1
    parsecWhitespace
    lenStr <- many1 digit
    let len = read lenStr :: Int
    parsecSkipManyTill "\n"
    parsecWhitespace
    list <- scan
    let lenList = length list
    when (lenList /= len) ((error . unpack)
        (  "Wrong number of tests parsed,"
        <> " expected: [" <> (textShow len) <> "]"
        <> " actual: [" <> (textShow lenList) <> "]"
        ))
    parsecWhitespace
    return (fromList list)
    where
        scan = recur <|> (return [])
        recur = do
            x <- oneTest prefix
            xs <- scan
            return (x:xs)

passedTestsCount :: Parser Int
passedTestsCount = do
    _ <- parsecLinePrefix "*** All remaining tests"
    parsecWhitespace
    parsecSkipLines 1
    parsecWhitespace
    numStr <- many1 digit
    return (read numStr :: Int)

results :: Parser Results
results = do
    tint <- listOfTests "*** INTERESTING tests" "[OK]"
    tfail <- listOfTests "*** FAILED tests" "[FAILED]"
    terr <- listOfTests "*** ERROR tests" "[ERROR]"
    count <- passedTestsCount
    return (Results count tint tfail terr)

parseResults :: Text -> IO Results
parseResults path = do
    res <- parsecParseFile results path
    return res
