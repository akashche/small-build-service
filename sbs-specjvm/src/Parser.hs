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
    ( parseSpecJVMOutput
    ) where

import Prelude ()
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy

import SBS.Common.Prelude
import SBS.Common.Parsec
import SBS.Common.Utils
import Data

benchMode :: Parser BenchMode
benchMode = do
    res <-
        (   (try (string "all") >> return All )
        <|> (try (string "avgt") >> return AverageTime )
        <|> (try (string "sample") >> return SampleTime )
        <|> (try (string "ss") >> return SingleShotTime )
        <|> (try (string "thrpt") >> return Throughput )
        <?> "BenchMode"
        )
    whitespace
    return res

benchUnit :: Parser BenchUnit
benchUnit = do
    skipOne (string "ops/")
    res <-
        (   (string "min" >> return OpsMin)
        <|> (string "s" >> return OpsSec)
        <|> (string "ms" >> return OpsMs)
        <?> "BenchUnit"
        )
    whitespace
    return res

benchResult :: Parser BenchResult
benchResult = do
    bname <- many1 (choice [alphaNum, (char '.')])
    whitespace
    bmode <- benchMode
    countString <- many1 digit
    let cnt = read countString :: Int
    whitespace
    scor <- floatAsInt
    skipOne (char '±')
    err <- floatAsInt
    whitespace
    bunits <- benchUnit
    let res = BenchResult (pack bname) bmode cnt scor err bunits
    whitespace
    return res

totalTimeSecs :: Parser Int
totalTimeSecs = do
    line <- lineContains "# Run complete. Total time:"
    let nums = Text.filter Char.isDigit line
    when (6 /= Text.length nums) (unexpected (unpack
        ("Error parsing 'Total time', nums found: [" <> nums <> "]")))
    let hoursString = Text.take 2 nums
    let hours = read (unpack hoursString) :: Int
    let minutesString = Text.take 2 (Text.drop 2 nums)
    let minutes = read (unpack minutesString) :: Int
    let secondsString = Text.take 2 (Text.drop 4 nums)
    let seconds = read (unpack secondsString) :: Int
    whitespace
    return ((hours * 3600) + (minutes * 60) + seconds)

specJVMResults :: Parser SpecJVMResults
specJVMResults = do
    time <- totalTimeSecs
    skipOne (manyTill anyChar newline)
    benchRes <- many1 benchResult
    let res = SpecJVMResults time (fromList benchRes)
    return res

parseSpecJVMOutput :: TextLazy.Text -> Text -> SpecJVMResults
parseSpecJVMOutput contents path =
    case parse specJVMResults (unpack path) contents of
        Left err -> errorText (errToText err)
        Right res -> res
