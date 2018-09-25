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

module Wilton ( ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Queries
import SBS.Common.Tier1
import SBS.Common.Utils
import SBS.Common.Wilton

import Lib

run :: Tier1Input -> IO ()
run (Tier1Input ctx cf) = do
    dyloadModules ["wilton_db", "wilton_process"]
    let tid = taskId ctx
    let db = dbConnection ctx
    let appd = appDir ctx
    let qdir = prependIfRelative appd (queriesDir ctx)
    qrs <- loadQueries (qdir <> "queries-tier1.sql")
    jid <- dbWithSyncTransaction db ( do
        jid <- createJob db qrs tid
        updateJobState db qrs jid StateRunning
        return jid )
    catch (spawnTestsAndWait cf appd) (\(e :: SomeException) -> do
        dbWithSyncTransaction db (finalizeJob db qrs jid StateError)
        errorText (showText e))
    dbWithSyncTransaction db (
        finalizeJob db qrs jid StateSuccess )
    return ()

-- tier1_all: run rec - spawn - run rec update - parse - results - run rec update - shortlog
-- tier1_test_db
-- tier1_test_spawn:
-- tier1_test_parse:
-- tier1_test_shortlog:

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    { errRun <- registerWiltonCall "tier1_run" run
    ; if isJust errRun then createWiltonError errRun

      else createWiltonError Nothing
    }

