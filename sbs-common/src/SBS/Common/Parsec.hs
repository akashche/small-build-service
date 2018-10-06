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

module SBS.Common.Parsec
    ( ParseError
    , (<|>), (<?>)
    , char, choice, eof, lookAhead, many, many1, manyTill, noneOf
    , oneOf, option, optional, sepBy, sepBy1, skipMany, try, unexpected
    -- Text.Parsec.Char
    , alphaNum, anyChar, digit, newline, string
    -- Text.Parsec.Error
    , Message(..), errorMessages, errorPos, messageString
    -- Text.Parsec.Pos
    , sourceColumn, sourceLine, sourceName
    -- Text.Parsec.Text.Lazy
    , Parser
    -- helpers
    , errToText
    -- combinators
    , floatAsInt, integer, lineContains, skipLines, skipLinesPrefix, skipLinesTill, skipManyTill, skipOne, whitespace
    -- strict text
    , parseText
    -- file
    , parseFile
    ) where

import Prelude ()
import Text.Parsec
    ( ParseError
    , (<|>), (<?>)
    , char, choice, eof, lookAhead, many, many1, manyTill, noneOf
    , oneOf, option, optional, parse, sepBy, sepBy1, skipMany, try, unexpected)
import Text.Parsec.Char (alphaNum, anyChar, digit, newline, string)
import Text.Parsec.Error (Message(..), errorMessages, errorPos, messageString)
import Text.Parsec.Pos (sourceColumn, sourceLine, sourceName)
import Text.Parsec.Text.Lazy (Parser)

import qualified Data.List as List
import qualified Data.Text as Text
import qualified Data.Text.Lazy as TextLazy

import SBS.Common.Prelude
import SBS.Common.Utils

-- general helpers

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
    TextLazy.toStrict (toLazyText
        (   fromText "ParseError:"
        <>  fromText " file: [" <> fromString (sourceName pos) <> fromText "],"
        <>  fromText " line: [" <> fromText (showText (sourceLine pos)) <> fromText "],"
        <>  fromText " column: [" <> fromText (showText (sourceColumn pos)) <> fromText "],"
        <>  fromText " messages: [" <> msg <> "]"
        ))
    where
        pos = errorPos err
        msgList = errorMessages err
        builderList = List.map errMsgToBuilder msgList
        commaList = List.intersperse (fromText ", ") builderList
        msg = List.foldl' (<>) (fromText "") commaList

-- helper combinators

integer :: Parser Int
integer = do
    valStr <- many1 digit
    let val = read valStr :: Int
    whitespace
    return val

floatAsInt :: Parser Int
floatAsInt = do
    headString <- many1 digit
    let head = read headString :: Int
    tail <- option
        0
        (do
            skipOne (char '.')
            tailString <- many1 digit
            let tail = read tailString :: Int
            return tail)
    let res = head * 1000 + tail
    whitespace
    return res

lineContains :: Text -> Parser Text
lineContains needle = do
    lineSt <- manyTill (noneOf ['\n']) (char '\n')
    let line = (pack lineSt)
    whitespace
    if Text.isInfixOf needle line
    then return line
    else lineContains needle

skipOne :: Parser a -> Parser ()
skipOne acomb = do
    _ <- acomb
    whitespace
    return ()

-- warning: all look-ahead data is kept in memory
skipManyTill :: Text -> Parser ()
skipManyTill end = do
    scan
    whitespace
    return ()
    where
        scan = done <|> recur
        done = do
            _ <- try (lookAhead (string (unpack end)))
            return ()
        recur = do
            _ <- anyChar
            scan
            return ()

skipLines :: Int -> Parser ()
skipLines count =
    if count > 0
    then do
        _ <- manyTill (noneOf ['\n']) (char '\n')
        whitespace
        skipLines (count - 1)
    else
        return ()

skipLinesPrefix :: Text -> Parser ()
skipLinesPrefix prefix = do
    lineSt <- manyTill (noneOf ['\n']) (char '\n')
    let line = (pack lineSt)
    if Text.isPrefixOf prefix (Text.stripStart line)
    then skipLinesPrefix prefix
    else do
        whitespace
        return ()

skipLinesTill :: Text -> Parser ()
skipLinesTill needle = do
    _ <- lineContains needle
    return ()

-- lexeme may be used instead
whitespace :: Parser ()
whitespace = skipMany (oneOf [' ', '\t', '\n', '\r'])

-- strict text

parseText :: Parser a -> Text -> a
parseText parser text =
    case parse parser "" (TextLazy.fromChunks [text]) of
        Left err -> (error . unpack) (errToText err)
        Right res -> res

-- file

parseFile :: Parser a -> Text -> IO a
parseFile parser path =
    withFileText path fun
    where
        parseContents contents =
            case parse parser (unpack path) contents of
                Left err -> (error . unpack) (errToText err)
                Right res -> res
        fun contents = return (parseContents contents)
