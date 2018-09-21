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
    ( parseJCStressOutput
    ) where

import Prelude ()
import qualified Data.List as List
import qualified Data.Text.Lazy as TextLazy

import SBS.Common.Prelude
import SBS.Common.Parsec
import SBS.Common.Utils

import Data

oneTest :: Text -> Parser Text
oneTest prefix = do
    skipOne (try (string (unpack prefix)))
    res <- manyTill (alphaNum <|> char '.') (char '\n')
    skipManyTill "\n\n"
    optional (skipOne (try (string "Messages:")) >> skipManyTill "\n\n")
    return (pack res)

listOfTests :: Text -> Text -> Parser (Vector Text)
listOfTests header prefix = do
    skipLinesTill header
    skipLines 1
    lenStr <- many1 digit
    let len = read lenStr :: Int
    skipManyTill "\n"
    list <- scan
    let lenList = List.length list
    when (lenList /= len) (errorText
        (  "Wrong number of tests parsed,"
        <> " expected: [" <> (showText len) <> "]"
        <> " actual: [" <> (showText lenList) <> "]"
        ))
    whitespace
    return (fromList list)
    where
        scan = recur <|> (return [])
        recur = do
            x <- oneTest prefix
            xs <- scan
            return (x:xs)

passedTestsCount :: Parser Int
passedTestsCount = do
    skipLinesTill "*** All remaining tests"
    skipLines 1
    numStr <- many1 digit
    return (read numStr :: Int)

jcstressResults :: Parser JCStressResults
jcstressResults = do
    tint <- listOfTests "*** INTERESTING tests" "[OK]"
    tfail <- listOfTests "*** FAILED tests" "[FAILED]"
    terr <- listOfTests "*** ERROR tests" "[ERROR]"
    count <- passedTestsCount
    return (JCStressResults count tint tfail terr)

parseJCStressOutput :: TextLazy.Text -> Text -> JCStressResults
parseJCStressOutput contents path =
    case parse jcstressResults (unpack path) contents of
        Left err -> errorText (errToText err)
        Right res -> res
