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
    ( specJVMResultsParser
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Parsec
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

totalTimeSecsFromLine :: Parser Int
totalTimeSecsFromLine = do
    skipOne (string "# Run complete. Total time:")
    hoursString <- many1 digit
    let hours = read hoursString :: Int
    skipOne (char ':')
    minutesString <- many1 digit
    let minutes = read minutesString :: Int
    skipOne (char ':')
    secondsString <- many1 digit
    let seconds = read secondsString :: Int
    return ((hours * 3600) + (minutes * 60) + seconds)

totalTimeSecs :: Parser Int
totalTimeSecs = do
    line <- lineContains "# Run complete. Total time:"
    let res = parseText totalTimeSecsFromLine line
    return res

specJVMResultsParser :: Parser SpecJVMResults
specJVMResultsParser = do
    time <- totalTimeSecs
    skipOne (manyTill anyChar newline)
    benchRes <- many1 benchResult
    let res = SpecJVMResults time (fromList benchRes)
    return res
