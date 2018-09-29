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
    ( spawnConfigureAndWait
    , spawnMakeAndWait
    ) where

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.JDKBuild
import SBS.Common.Wilton

import Data

spawnConfigureAndWait :: JDKBuildConfig -> Paths -> IO ()
spawnConfigureAndWait cf paths = do
    code <- spawnProcess SpawnedProcessArgs
        { workDir = buildDir (paths :: Paths)
        , executable = bashPath (paths :: Paths)
        , execArgs = fromList [ "-c", argsstr ]
        , outputFile = log
        , awaitExit = True
        }
    checkSpawnSuccess "jdkbuild_conf" code log
    return ()
    where
        log = confOutPath (paths :: Paths)
        args = Vector.concat [fromList
            [ sourceDir (paths :: Paths) <> "configure"
            , "--with-boot-jdk=" <> (bootJdkDir (paths :: Paths))
            , "--with-jtreg=" <> (jtregDir (paths :: Paths))
            , "--with-log=" <> (logLevel cf)
            ], additionalConfigureArguments cf]
        folder ac el = ac <> " " <> el
        -- https://help.appveyor.com/discussions/problems/4150-0-bad-file-descriptor-error-running-configure
        argsstr = "cat /dev/null | bash " <> (Vector.foldl1' folder args)

spawnMakeAndWait :: JDKBuildConfig -> Paths -> IO ()
spawnMakeAndWait cf paths = do
    code <- spawnProcess SpawnedProcessArgs
        { workDir = buildDir (paths :: Paths)
        , executable = makePath (paths :: Paths)
        , execArgs = fromList [target cf]
        , outputFile = log
        , awaitExit = True
        }
    checkSpawnSuccess "jdkbuild_make" code log
    return ()
    where
        log = makeOutPath (paths :: Paths)
