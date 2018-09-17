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

module SBS.Common.Queries
    ( Queries
    , loadQueries
    ) where

import Prelude ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import SBS.Common.Prelude
import SBS.Common.Parsec
import SBS.Common.Utils

type Queries = HashMap Text Text

singleQuery :: Parser (Text, Text)
singleQuery = do
    skipOne (string "/**")
    name <- many1 alphaNum
    whitespace
    skipOne (string "*/")
    value <- manyTill anyChar
        (   (try (lookAhead (string "/**")) >> return ())
        <|> eof
        )
    return ((convert name), (convert value))
    where
        convert st = Text.dropWhileEnd isEol (pack st)
        isEol ch = '\n' == ch

queries :: Parser Queries
queries = do
    skipLinesPrefix "--"
    skipManyTill "/**"
    li <- many1 singleQuery
    return (HashMap.fromList li)

loadQueries :: Text -> IO Queries
loadQueries path =
    withFileText path fun
    where
        fun contents =
            case parse queries (unpack path) contents of
                Left err -> errorText (errToText err)
                Right res -> return res
