
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( hello
    ) where

import Prelude
    ( Int, IO, Maybe(Just, Nothing), Show
    , return
    )
import Data.Aeson as Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Maybe (isJust)
import Data.Text (Text)
import Foreign.Wilton.FFI (invokeWiltonCall)
import GHC.Generics (Generic)

import SBS.Common.Data (Empty(Empty))
import SBS.Common.JCStress (MyObjOut(..))

hello :: Empty -> IO MyObjOut
hello _ =
    return (MyObjOut "foo" 43)


