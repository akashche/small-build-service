
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

diffResults :: JCStressResults -> JCStressResults -> JCStressResultsDiff
diffResults baseline res =
    JCStressResultsDiff pd xd fd ed
    where
        pd = (passedCount res) - (passedCount baseline)
        xd = (Vector.length (interesting res)) - (Vector.length (interesting baseline))
        fd = (Vector.length (failed res)) - (Vector.length (failed baseline))
        ed = (Vector.length (error res)) - (Vector.length (error baseline))

