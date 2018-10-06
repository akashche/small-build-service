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

module Spawn
    ( spawnSpecAndWait
    ) where

import Prelude ()
import qualified Data.Text as Text
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.SpecJVM
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

spawnSpecAndWait :: SpecJVMConfig -> Paths -> IO ()
spawnSpecAndWait cf paths = do
    code <- spawnProcess SpawnedProcessArgs
        { workDir = workDir (paths :: Paths)
        , executable = execPath (paths :: Paths)
        , execArgs = fromList
            [  ("-Xmx" <> (showText (xmxMemoryLimitMB cf)) <> "M")
            , "-jar", specjvmJarPath (paths :: Paths)
            , "-t", showText (threadsCount cf)
            , "-e", exreg (excludedBenchmarks cf)
            ]
        , outputFile = log
        , awaitExit = True
        }
    checkSpawnSuccess "specjvm" code log
    return ()
    where
        log = outputPath (paths :: Paths)
        sepNonEmpty st = if Text.length st > 0 then st <> "|" else st
        folder ac el = (sepNonEmpty ac) <> (Text.replace "." "\\." el)
        exreg vec = Vector.foldl' folder "" vec

