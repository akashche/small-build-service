
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Lib
    ( hello
    , diffResults
    ) where

import Prelude ()
import qualified Data.Vector as Vector

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JCStress

import Data

hello :: Empty -> IO MyObjOut
hello _ =
    return (MyObjOut "foo" 43)

diffResults :: JCStressResults -> JCStressResults -> JCStressResultsDiff
diffResults baseline res =
    JCStressResultsDiff pd id fd ed
    where
        pd = (passedCount res) - (passedCount baseline)
        id = (Vector.length (interested res)) - (Vector.length (interested baseline))
        fd = (Vector.length (failed res)) - (Vector.length (failed baseline))
        ed = (Vector.length (errored res)) - (Vector.length (errored baseline))
