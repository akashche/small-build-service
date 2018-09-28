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

import Prelude ()
import qualified Data.HashMap.Strict as HashMap

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Parsec
import SBS.Common.Queries
import SBS.Common.Utils

testParser :: Parser Text
testParser = do
    foo <- string "foo"
    return (pack foo)

main :: IO ()
main = do
    -- Data
    unless ("{}" == encodeJsonText Empty) (error "empty fail")

    -- Utils

    -- prelude text
    unless ("foo" == showText ("foo" :: Text)) (error "showText fail")
    unless ("foo" == showText ("foo" :: String)) (error "showText fail")
    unless ("foo" == showText ("foo" :: ByteString)) (error "showText fail")

    -- datetime
    unless ("2018-09-09 19:15:11" == formatISO8601 (parseISO8601 "2018-09-09 19:15:11")) (error "ISO8601 fail")

    -- paths
    unless ("foobar" == prependIfRelative "foo" "bar") (error "prepend fail")
    unless ("/bar" == prependIfRelative "foo" "/bar") (error "prepend fail")
    unless ("c:\\bar" == prependIfRelative "foo" "c:\\bar") (error "prepend fail")
    unless ("c:/bar" == prependIfRelative "foo" "c:/bar") (error "prepend fail")

    -- queries
    qrs <- loadQueries "../resources/queries-main.sql"
    unless (4 == HashMap.size qrs) (error "Queries fail")

    -- Parsec
    unless ("foo" == parseText testParser "foo") (error "parser fail")

    putStrLn "Tests Passed."
    return ()
