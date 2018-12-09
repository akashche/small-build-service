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

floatAsInt :: Parser Int
floatAsInt = do
    headString <- many1 digit
    let head = read headString :: Int
    tail <- option
        0
        (do
            _ <- char '.'
            tailString <- many1 digit
            let tail = read tailString :: Int
            return tail)
    let res = head * 1000 + tail
    parsecWhitespace
    return res

benchMode :: Parser BenchMode
benchMode = do
    res <-
        (   (parsecTry (string "all") >> return All )
        <|> (parsecTry (string "avgt") >> return AverageTime )
        <|> (parsecTry (string "sample") >> return SampleTime )
        <|> (parsecTry (string "ss") >> return SingleShotTime )
        <|> (parsecTry (string "thrpt") >> return Throughput )
        <?> "BenchMode"
        )
    parsecWhitespace
    return res

benchUnit :: Parser BenchUnit
benchUnit = do
    _ <- string "ops/"
    res <-
        (   (string "min" >> return OpsMin)
        <|> (string "s" >> return OpsSec)
        <|> (string "ms" >> return OpsMs)
        <?> "BenchUnit"
        )
    parsecWhitespace
    return res

benchResult :: Parser BenchResult
benchResult = do
    bname <- many1 (choice [alphaNum, (char '.')])
    parsecWhitespace
    bmode <- benchMode
    countString <- many1 digit
    let cnt = read countString :: Int
    parsecWhitespace
    scor <- floatAsInt
    _ <- char '±'
    parsecWhitespace
    err <- floatAsInt
    parsecWhitespace
    bunits <- benchUnit
    let res = BenchResult (pack bname) bmode cnt scor err bunits
    parsecWhitespace
    return res

totalTimeSecsFromLine :: Parser Int
totalTimeSecsFromLine = do
    _ <- string "# Run complete. Total time:"
    parsecWhitespace
    hoursString <- many1 digit
    let hours = read hoursString :: Int
    _ <- char ':'
    minutesString <- many1 digit
    let minutes = read minutesString :: Int
    _ <- char ':'
    secondsString <- many1 digit
    let seconds = read secondsString :: Int
    return ((hours * 3600) + (minutes * 60) + seconds)

totalTimeSecs :: Parser Int
totalTimeSecs = do
    line <- parsecLineContains "# Run complete. Total time:"
    parsecWhitespace
    let res = parsecParseText totalTimeSecsFromLine line
    return res

results :: Parser Results
results = do
    time <- totalTimeSecs
    _ <- manyTill anyChar newline
    benchRes <- many1 benchResult
    let res = Results time (fromList benchRes)
    return res

summary :: Parser Text
summary = do
    headline <- parsecLineContains "# Run complete. Total time:"
    parsecWhitespace
    st <- many1 anyChar
    return (headline <> "\n\n" <> (pack st))

parseResults :: Text -> IO Results
parseResults path = do
    res <- parsecParseFile results path
    return res

parseSummary :: Text -> IO Text
parseSummary path = do
    res <- parsecParseFile summary path
    return res
