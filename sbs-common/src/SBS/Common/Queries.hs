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

module SBS.Common.Queries
    ( resolveQueriesPath
      -- re-export
    , Queries
    , queriesLoad
    ) where

import Prelude ()
import VtUtils.Prelude
import VtUtils.Queries

import SBS.Common.Data

resolveQueriesPath :: DiffRequest -> Text -> Text
resolveQueriesPath req postfix =
    pathConcat (pathPrepend appd qdir) ("queries-" <> postfix <> ".sql")
    where
        appd = appDir (req :: DiffRequest)
        qdir = queriesDir (req :: DiffRequest)
