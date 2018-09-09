
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

