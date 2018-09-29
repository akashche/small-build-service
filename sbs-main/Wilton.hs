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
import SBS.Common.JDKBuild
import SBS.Common.Tier1
import SBS.Common.Wilton

import Data
import DB
import Lib

run :: Vector Text -> IO ()
run arguments = do
    (cf, db, qrs) <- initApp arguments
    ctx <- initTask cf db qrs
    when (enabled (jdkbuild cf :: JDKBuildConfig))
        (wiltoncall "jdkbuild_run" (JDKBuildInput ctx (jdkbuild cf) eim))
    when (enabled (tier1 cf :: Tier1Config))
        (wiltoncall "tier1_run" (Tier1Input ctx (tier1 cf)))
    finalizeTask db qrs (taskId ctx)
    putStrLn "Run finished"
    return ()
    where
        eim = "" -- TODO

runMock :: Vector Text -> IO ()
runMock arguments = do
    (cf, db, qrs) <- initApp arguments
    ctx <- initTask cf db qrs
    wiltoncall "jdkbuild_run_mock" (JDKBuildInput ctx (jdkbuild cf) "") :: IO ()
    wiltoncall "tier1_run_mock" (Tier1Input ctx (tier1 cf)) :: IO ()
    finalizeTask db qrs (taskId ctx)
    putStrLn "MOCK Run finished"
    return ()


foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    {           errRun <- registerWiltonCall "run" run
    ; if isJust errRun then createWiltonError errRun

    ; else do { errRunMock <- registerWiltonCall "run_mock" runMock
    ; if isJust errRunMock then createWiltonError errRunMock

      else createWiltonError Nothing
    }}
