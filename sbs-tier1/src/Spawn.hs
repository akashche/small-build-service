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
    ( spawnTestsAndWait
    ) where

import Prelude ()
-- import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Wilton

import Data

spawnTestsAndWait :: Paths -> Vector Text -> IO ()
spawnTestsAndWait paths args = do
    _code <- spawnProcess SpawnedProcessArgs
        { workDir = buildDir (paths :: Paths)
        , executable = execPath paths
        , execArgs = args
        , outputFile = outputPath (paths :: Paths)
        , awaitExit = True
        }
    -- todo: add less strict check
    -- checkSpawnSuccess "tier1" code log
    return ()
