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
import VtUtils.Prelude
import qualified Data.List as List
import qualified Data.Text as Text

import SBS.Common.Data
import SBS.Common.JDKBuild

import Data

resolvePaths :: TaskContext -> JDKBuildConfig -> Paths
resolvePaths ctx cf = Paths
    { workDir = wd
    , sourceDir = pathPrepend appd (sourceDir (cf :: JDKBuildConfig))
    , bootJdkDir = pathPrepend appd (bootJdkDir (cf :: JDKBuildConfig))
    , jtregDir = pathPrepend appd (jtregDir (cf :: JDKBuildConfig))
    , buildDir = pathPrepend appd (buildDir (cf :: JDKBuildConfig))
    , hgPath = pathPrepend appd (hgPath (cf :: JDKBuildConfig))
    , bashPath = pathPrepend appd (bashPath (cf :: JDKBuildConfig))
    , makePath = pathPrepend appd (makePath (cf :: JDKBuildConfig))
    , confOutPath = pathConcat wd (confOutputFile (cf :: JDKBuildConfig))
    , makeOutPath = pathConcat wd (makeOutputFile (cf :: JDKBuildConfig))
    , repoUrlOutPath = pathConcat wd "repourl.log"
    , repoRevOutPath = pathConcat wd "revision.log"
    , mockOutputDir = pathPrepend appd (mockOutputDir (cf :: JDKBuildConfig))
    , queriesPath = pathConcat (pathPrepend appd (queriesDir (ctx :: TaskContext))) "queries-jdkbuild.sql"
    }
    where
        appd = appDir (ctx :: TaskContext)
        wd = pathPrepend appd (workDir (cf :: JDKBuildConfig))

mockCtx :: Text -> TaskContext
mockCtx appd = TaskContext
    { taskId = 42
    , dbConnection = DBConnection 43 44
    , appDir = appd
    , queriesDir = "queries"
    , destBaseDir = ""
    , destDir = ""
    }

mockConfig :: JDKBuildConfig
mockConfig = JDKBuildConfig
    { enabled = True
    , workDir = ""
    , mockOutputDir = "mock"
    , confOutputFile = "conf.log"
    , makeOutputFile = "make.log"
    , sourceDir = "jdk"
    , buildDir = "jdk"
    , bootJdkDir = "bootjdk"
    , jtregDir = "jtreg"
    , bashPath = "/bin/bash"
    , hgPath = "/usr/bin/hg"
    , makePath = "/usr/bin/make"
    , logLevel = "info"
    , additionalConfigureArguments = fromList ["--disable-warnings-as-errors"]
    , target = "images"
    }

mockPaths :: Text -> Paths
mockPaths appd = resolvePaths (mockCtx appd) mockConfig

-- note: deliberately errors on invalid input
resolveDestDir :: Paths -> Text -> IO Text
resolveDestDir paths base = do
    urlfull <- readFile (unpack (repoUrlOutPath paths))
    let url = Text.strip urlfull
    revfull <- readFile (unpack (repoRevOutPath paths))
    let rev = Text.strip revfull
    let res = pathConcat base (dest url rev)
    return res
    where
        filfun el = Text.length el > 0
        nonempty parts = List.filter filfun parts
        tail parts = List.drop ((List.length parts) - 2) parts
        conc parts = (List.head parts) <> "_" <> (List.last parts)
        dest url rev = (conc (tail (nonempty (Text.splitOn "/" url)))) <> "_" <> rev

