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
    ( parseConfOutput
    , parseMakeOutput
    ) where

import Prelude ()
import VtUtils.Prelude

import SBS.Common.Parsec

import Data

configureDetails :: Parser ConfigureDetails
configureDetails = do
    _ <- parsecLinePrefix "A new configuration has been successfully created in"
    parsecWhitespace
    dir <- manyTill (noneOf ['\n']) (char '\n')
    return (ConfigureDetails (pack dir))

parseConfOutput :: Text -> IO ConfigureDetails
parseConfOutput path = do
    res <- parsecParseFile configureDetails path
    return res

copyingLine :: Parser Text
copyingLine = do
    _ <- string "Copying"
    parsecWhitespace
    imagesDir <- many1 alphaNum
    _ <- char '/'
    jdkDir <- many1 alphaNum
    let res = pathConcat (pack imagesDir) (pack jdkDir)
    return res

makeDetails :: Parser MakeDetails
makeDetails = do
    _ <- parsecLinePrefix "Creating jdk image"
    parsecWhitespace
    line <- parsecLinePrefix "Copying"
    let dir = parsecParseText copyingLine line
    return (MakeDetails dir)

parseMakeOutput :: Text -> IO MakeDetails
parseMakeOutput path = do
    res <- parsecParseFile makeDetails path
    return res
