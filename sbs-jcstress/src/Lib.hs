
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( ) where

import Prelude
    ( Int, IO, Maybe(Just, Nothing), Show
    , return
    )
import Data.Aeson as Aeson (FromJSON, ToJSON)
import Data.Data (Data, Typeable)
import Data.Maybe (isJust)
import Data.Text (Text)
import Foreign.C.String (CString)
import Foreign.Wilton.FFI (createWiltonError, registerWiltonCall, invokeWiltonCall)
import GHC.Generics (Generic)

import SBS.Common.Data (Empty(Empty))
import SBS.Common.JCStress (MyObjOut, createMyObjOut)


hello :: Empty -> IO MyObjOut
hello _ =
    return (createMyObjOut "foo" 43)

-- this function is called on module load

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    { errMain <- registerWiltonCall "sbs_jcstress_hello" hello
    ; if isJust errMain then createWiltonError errMain
      else createWiltonError Nothing
    }
