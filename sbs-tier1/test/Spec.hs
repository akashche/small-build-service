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

main :: IO ()
main = do
    _res <- parseTier1File "test/tier1.log"
--     putStrLn (showText res)

    let res1 = fromList
            [ TestSuite "foo" 1 2 3
            , TestSuite "bar" 2 3 1
            , TestSuite "baz" 2 3 1
            ]
    let res2 = fromList
            [ TestSuite "foo" 4 1 2
            , TestSuite "bar" 3 1 4
            ]
    let diff = diffTwoResults res1 res2
    putStrLn (showText diff)


    putStrLn "Tests Passed."
    return ()

