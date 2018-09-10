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
    return (pack res)

listOfTests :: Text -> Text -> Parser (Vector Text)
listOfTests header prefix = do
    skipManyTill header
    skipLines 3
    list <- scan
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
    skipManyTill "*** All remaining tests\n"
    skipLines 2
    numStr <- many1 digit
    return (read numStr :: Int)

jcstressResults :: Parser JCStressResults
jcstressResults = do
    tint <- listOfTests "*** INTERESTING tests\n" "[OK]"
    tfail <- listOfTests "*** FAILED tests\n" "[FAILED]"
    terr <- listOfTests "*** ERROR tests\n" "[ERROR]"
    count <- passedTestsCount
    return (JCStressResults count tint tfail terr)

parseJCStressOutput :: TextLazy.Text -> Text -> JCStressResults
parseJCStressOutput contents path =
    case parse jcstressResults (unpack path) contents of
        Left err -> errorText (errToText err)
        Right res -> res
