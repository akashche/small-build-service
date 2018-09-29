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

module Lib
    ( resolvePaths
    , mockPaths
    , readRepoUrl
    , readRepoRevision
    ) where

import Prelude ()
import qualified Data.Text as Text

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JDKBuild
import SBS.Common.Utils
import SBS.Common.Wilton

import Data

resolvePaths :: TaskContext -> JDKBuildConfig -> Paths
resolvePaths ctx cf = Paths
    { workDir = wd
    , sourceDir = prependIfRelative appd (sourceDir (cf :: JDKBuildConfig))
    , bootJdkDir = prependIfRelative appd (bootJdkDir (cf :: JDKBuildConfig))
    , jtregDir = prependIfRelative appd (jtregDir (cf :: JDKBuildConfig))
    , buildDir = wd <> (buildDir (cf :: JDKBuildConfig))
    , hgPath = prependIfRelative appd (hgPath (cf :: JDKBuildConfig))
    , bashPath = prependIfRelative appd (bashPath (cf :: JDKBuildConfig))
    , makePath = prependIfRelative appd (makePath (cf :: JDKBuildConfig))
    , confOutPath = wd <> "conf.log"
    , makeOutPath = wd <> "make.log"
    , mockOutputDir = prependIfRelative appd (mockOutputDir (cf :: JDKBuildConfig))
    , queriesPath = prependIfRelative appd (queriesDir ctx) <> "queries-jdkbuild.sql"
    }
    where
        appd = appDir ctx
        wd = prependIfRelative appd (workDir (cf :: JDKBuildConfig))

mockPaths :: Paths
mockPaths = Paths
    { workDir = ""
    , sourceDir = ""
    , bootJdkDir = ""
    , jtregDir =  ""
    , buildDir =  ""
    , hgPath = ""
    , bashPath = ""
    , makePath = ""
    , confOutPath = ""
    , makeOutPath = ""
    , mockOutputDir = ""
    , queriesPath = ""
    }

readRepoUrl :: Paths -> IO Text
readRepoUrl paths = do
    code <- spawnProcess SpawnedProcessArgs
        { workDir = sourceDir (paths :: Paths)
        , executable = hgPath (paths :: Paths)
        , execArgs = fromList [ "paths", "default"]
        , outputFile = log
        , awaitExit = True
        }
    checkSpawnSuccess "jdkbuild_repourl" code log
    url <- readFile (unpack log)
    return (Text.strip url)
    where
        wd = workDir (paths :: Paths)
        log = wd <> "repourl.log"

readRepoRevision :: Paths -> IO Text
readRepoRevision paths = do
    code <- spawnProcess SpawnedProcessArgs
        { workDir = sourceDir (paths :: Paths)
        , executable = hgPath (paths :: Paths)
        , execArgs = fromList [ "id", "-i"]
        , outputFile = log
        , awaitExit = True
        }
    checkSpawnSuccess "jdkbuild_revision" code log
    rev <- readFile (unpack log)
    return (Text.strip rev)
    where
        wd = workDir (paths :: Paths)
        log = wd <> "revision.log"
