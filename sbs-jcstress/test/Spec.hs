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
import Lib
import Parser

parseLog :: Text -> IO JCStressResults
parseLog path =
    withFileText path fun
    where
        fun tx = return (parseJCStressOutput tx path)

main :: IO ()
main = do
    baseline <- parseLog "test/jcstress_abridged.log"
    putStrLn (showText baseline)
    res <- parseLog "test/jcstress_abridged_alt.log"
    let diff = diffResults baseline res
    putStrLn (showText diff)

    putStrLn "Tests Passed."
    return ()
