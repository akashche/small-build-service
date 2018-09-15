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

import SBS.Common.Prelude
import SBS.Common.Utils

import Data
import Parser

parseConf :: Text -> IO ConfigureDetails
parseConf path =
    withFileText path fun
    where
        fun tx = return (parseConfigureOutput tx path)

parseMake :: Text -> IO MakeDetails
parseMake path =
    withFileText path fun
    where
        fun tx = return (parseMakeOutput tx path)

main :: IO ()
main = do
    -- conf.log
    cd <- parseConf "test/conf.log"
--     unless ("foo" == showText ("foo" :: Text)) (errorText "showText fail")
    putStrLn (showText cd)

    -- make.log
    md <- parseMake "test/make.log"
    putStrLn (showText md)

    putStrLn "Tests Passed."
    return ()

