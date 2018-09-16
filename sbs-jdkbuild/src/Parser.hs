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
    ( parseConfigureOutput
    , parseMakeOutput
    ) where

import Prelude ()
import qualified Data.Text.Lazy as TextLazy

import SBS.Common.Prelude
import SBS.Common.Parsec
import SBS.Common.Utils

import Data

configureDetails :: Parser ConfigureDetails
configureDetails = do
    skipManyTill "A new configuration has been successfully created in"
    skipLines 1
    dir <- manyTill (noneOf ['\n']) (char '\n')
    return (ConfigureDetails ((pack dir) <> "/"))

parseConfigureOutput :: TextLazy.Text -> Text -> ConfigureDetails
parseConfigureOutput contents path =
    case parse configureDetails (unpack path) contents of
        Left err -> errorText (errToText err)
        Right res -> res

makeDetails :: Parser MakeDetails
makeDetails = do
    skipManyTill "Creating jdk image"
    skipLines 1
    skipOne (string "Copying")
    whitespace
    imagesDir <- many1 alphaNum
    skipOne (char '/')
    jdkDir <- many1 alphaNum
    let dir = (pack imagesDir) <> "/" <> (pack jdkDir) <> "/"
    return (MakeDetails dir)

parseMakeOutput :: TextLazy.Text -> Text -> MakeDetails
parseMakeOutput contents path =
    case parse makeDetails (unpack path) contents of
        Left err -> errorText (errToText err)
        Right res -> res

