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
    ( configureDetailsParser
    , makeDetailsParser
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Parsec

import Data

configureDetailsParser :: Parser ConfigureDetails
configureDetailsParser = do
    skipLinesTill "A new configuration has been successfully created in"
    dir <- manyTill (noneOf ['\n']) (char '\n')
    return (ConfigureDetails ((pack dir) <> "/"))

makeDetailsParser :: Parser MakeDetails
makeDetailsParser = do
    skipLinesTill "Creating jdk image"
    skipOne (string "Copying")
    whitespace
    imagesDir <- many1 alphaNum
    skipOne (char '/')
    jdkDir <- many1 alphaNum
    let dir = (pack imagesDir) <> "/" <> (pack jdkDir) <> "/"
    return (MakeDetails dir)
