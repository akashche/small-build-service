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
    , mockCtx
    , mockConfig
    , mockPaths
    , resolveDestDir
    ) where

import Prelude ()
import qualified Data.List as List
import qualified Data.Text as Text

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JDKBuild
import SBS.Common.Utils

import Data

resolvePaths :: TaskContext -> JDKBuildConfig -> Paths
resolvePaths ctx cf = Paths
    { workDir = wd
    , sourceDir = prependIfRelative appd (sourceDir (cf :: JDKBuildConfig))
    , bootJdkDir = prependIfRelative appd (bootJdkDir (cf :: JDKBuildConfig))
    , jtregDir = prependIfRelative appd (jtregDir (cf :: JDKBuildConfig))
    , buildDir = prependIfRelative appd (buildDir (cf :: JDKBuildConfig))
    , hgPath = prependIfRelative appd (hgPath (cf :: JDKBuildConfig))
    , bashPath = prependIfRelative appd (bashPath (cf :: JDKBuildConfig))
    , makePath = prependIfRelative appd (makePath (cf :: JDKBuildConfig))
    , confOutPath = wd <> confOutputFile (cf :: JDKBuildConfig)
    , makeOutPath = wd <> makeOutputFile (cf :: JDKBuildConfig)
    , repoUrlOutPath = wd <> "repourl.log"
    , repoRevOutPath = wd <> "revision.log"
    , mockOutputDir = prependIfRelative appd (mockOutputDir (cf :: JDKBuildConfig))
    , queriesPath = prependIfRelative appd (queriesDir (ctx :: TaskContext)) <> "queries-jdkbuild.sql"
    }
    where
        appd = appDir (ctx :: TaskContext)
        wd = prependIfRelative appd (workDir (cf :: JDKBuildConfig))

mockCtx :: Text -> TaskContext
mockCtx appd = TaskContext
    { taskId = 42
    , dbConnection = DBConnection 43 44
    , appDir = appd
    , queriesDir = "queries/"
    , destBaseDir = ""
    , destDir = ""
    }

mockConfig :: JDKBuildConfig
mockConfig = JDKBuildConfig
    { enabled = True
    , workDir = ""
    , mockOutputDir = "mock/"
    , confOutputFile = "conf.log"
    , makeOutputFile = "make.log"
    , sourceDir = "jdk/"
    , buildDir = "jdk/"
    , bootJdkDir = "bootjdk/"
    , jtregDir = "jtreg/"
    , bashPath = "/bin/bash"
    , hgPath = "/usr/bin/hg"
    , makePath = "/usr/bin/make"
    , logLevel = "info"
    , additionalConfigureArguments = fromList ["--disable-warnings-as-errors"]
    , target = "images"
    }

mockPaths :: Text -> Paths
mockPaths appd = resolvePaths (mockCtx (appd <> "/")) mockConfig

-- note: deliberately errors on invalid input
resolveDestDir :: Paths -> Text -> IO Text
resolveDestDir paths base = do
    urlfull <- readFile (unpack (repoUrlOutPath paths))
    let url = Text.strip urlfull
    return (base <> (dest url) <> "/")
    where
        filfun el = Text.length el > 0
        nonempty parts = List.filter filfun parts
        tail parts = List.drop ((List.length parts) - 2) parts
        conc parts = (List.head parts) <> "_" <> (List.last parts)
        dest url = conc (tail (nonempty (Text.splitOn "/" url)))

