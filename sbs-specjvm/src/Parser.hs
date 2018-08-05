
{-# LANGUAGE OverloadedStrings #-}

module Parser
    ( parseSpecJVMOutput
    ) where

import Prelude
    ( Either(Left, Right), Int
    , (+), (*), (>>)
    , map, read, return, show
    )
import Data.List (foldl', intersperse)
import Data.Monoid ((<>))
import Data.Text (Text, pack, unpack)
import Data.Text.Lazy (toStrict)
import qualified Data.Text.Lazy as Lazy (Text)
import Data.Text.Lazy.Builder (Builder, fromText, fromString, toLazyText)
import Data.Vector (fromList)
import Text.Parsec
    ( ParseError
    , (<|>), (<?>)
    , char, choice, many1, manyTill, oneOf, option, parse, skipMany, try)
import Text.Parsec.Char (alphaNum, anyChar, digit, newline, string)
import Text.Parsec.Error (Message(..), errorMessages, errorPos, messageString)
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)
import Text.Parsec.Text.Lazy (Parser)

import Data (BenchMode(..), BenchResult(..), BenchUnit(..), SpecJVMResults(..))

-- helpers

errMsgToBuilder :: Message -> Builder
errMsgToBuilder msg =
    fromText prefix <> fromString (messageString msg)
    where
        prefix = case msg of
            (SysUnExpect _) -> "unexpected: "
            (UnExpect _) -> "unexpected: "
            (Expect _) -> "expected: "
            (Message _) -> "message: "

errToText :: ParseError -> Text
errToText err =
    toStrict (toLazyText
        (   fromText "ParseError:"
        <>  fromText " file: [" <> fromString (sourceName pos) <> fromText "],"
        <>  fromText " line: [" <> fromString (show (sourceLine pos)) <> fromText "],"
        <>  fromText " column: [" <> fromString (show (sourceColumn pos)) <> fromText "],"
        <>  fromText " messages: [" <> msg <> "]"
        ))
    where
        pos = errorPos err
        msgList = errorMessages err
        builderList = map errMsgToBuilder msgList
        commaList = intersperse (fromText ", ") builderList
        msg = foldl' (<>) (fromText "") commaList

-- lexeme may be used instead
whitespace :: Parser ()
whitespace = skipMany (oneOf [' ', '\t', '\n', '\r'])

-- parsers

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
    string "ops/"
    res <-
        (   (string "min" >> return OpsMin)
        <|> (string "s" >> return OpsSec)
        <|> (string "ms" >> return OpsMs)
        <?> "BenchUnit"
        )
    whitespace
    return res

floatAsInt :: Parser Int
floatAsInt = do
    headString <- many1 digit
    let head = read headString :: Int
    tail <- option
        0
        (do
            char '.'
            tailString <- many1 digit
            let tail = read tailString :: Int
            return tail)
    let res = head * 1000 + tail
    whitespace
    return res

benchResult :: Parser BenchResult
benchResult = do
    name <- many1 (choice [alphaNum, (char '.')])
    whitespace
    mode <- benchMode
    countString <- many1 digit
    let count = read countString :: Int
    whitespace
    score <- floatAsInt
    char '±'
    whitespace
    error <- floatAsInt
    whitespace
    units <- benchUnit
    let res = BenchResult (pack name) mode count score error units
    whitespace
    return res

totalTimeSecs :: Parser Int
totalTimeSecs = do
    string "Total time:"
    whitespace
    hoursString <- many1 digit
    let hours = read hoursString :: Int
    char ':'
    minutesString <- many1 digit
    let minutes = read minutesString :: Int
    char ':'
    secondsString <- many1 digit
    let seconds = read secondsString :: Int
    whitespace
    return ((hours * 3600) + (minutes * 60) + seconds)

specJVMResults :: Parser SpecJVMResults
specJVMResults = do
    manyTill anyChar (try (string "# Run complete."))
    whitespace
    time <- totalTimeSecs
    manyTill anyChar newline
    benchRes <- many1 benchResult
    let res = SpecJVMResults time (fromList benchRes)
    return res

parseSpecJVMOutput :: Lazy.Text -> Text -> Either Text SpecJVMResults
parseSpecJVMOutput contents path =
    case parse specJVMResults (unpack path) contents of
        Left err -> Left (errToText err)
        Right res -> Right res
