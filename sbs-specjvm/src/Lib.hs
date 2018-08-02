
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( hello
    ) where

import Prelude
    ( IO
    , return
    )
import Foreign.Wilton.FFI (invokeWiltonCall)

import SBS.Common.Data (Empty(..))
import SBS.Common.JCStress (MyObjOut(..))

hello :: Empty -> IO MyObjOut
hello _ =
    return (MyObjOut "foo" 43)

