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
import SBS.Common.Parsec
import SBS.Common.Utils

import Diff
import Parser


main :: IO ()
main = do
    baseline <- parseFile jcstressResultsParser "test/jcstress_abridged.log"
    putStrLn (showText baseline)
    res <- parseFile jcstressResultsParser "test/jcstress_abridged_alt.log"
    let diff = diffResults baseline res
    putStrLn (showText diff)

    putStrLn "Tests Passed."
    return ()
