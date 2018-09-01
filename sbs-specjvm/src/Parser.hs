
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Parser
    ( parseSpecJVMOutput
    ) where

import Prelude ()
import qualified Data.Text.Lazy as TextLazy

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

totalTimeSecs :: Parser Int
totalTimeSecs = do
    skipOne (string "# Run complete. Total time:")
    hoursString <- many1 digit
    let hours = read hoursString :: Int
    skipOne (char ':')
    minutesString <- many1 digit
    let minutes = read minutesString :: Int
    skipOne (char ':')
    secondsString <- many1 digit
    let seconds = read secondsString :: Int
    whitespace
    return ((hours * 3600) + (minutes * 60) + seconds)

specJVMResults :: Parser SpecJVMResults
specJVMResults = do
    skipManyTill "# Run complete."
    time <- totalTimeSecs
    skipOne (manyTill anyChar newline)
    benchRes <- many1 benchResult
    let res = SpecJVMResults time (fromList benchRes)
    return res

parseSpecJVMOutput :: TextLazy.Text -> Text -> Either Text SpecJVMResults
parseSpecJVMOutput contents path =
    case parse specJVMResults (unpack path) contents of
        Left err -> Left (errToText err)
        Right res -> Right res
