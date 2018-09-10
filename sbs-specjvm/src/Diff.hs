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

module Diff
    ( diffResults
    ) where

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import Data

diffBench :: BenchResult -> BenchResult -> BenchDiff
diffBench baseline res =
    BenchDiff (name (baseline :: BenchResult)) diff
    where diff = div ((score res) * 100) (score baseline)

diffResults :: SpecJVMResults -> SpecJVMResults -> SpecJVMResultsDiff
diffResults baseline res =
    SpecJVMResultsDiff relTime benches
    where
        relTime = div ((totalTimeSeconds res) * 100) (totalTimeSeconds baseline)
        benches = Vector.zipWith diffBench bsBase bsRes
        bsBase = benchmarks (baseline :: SpecJVMResults)
        bsRes = benchmarks (res :: SpecJVMResults )

