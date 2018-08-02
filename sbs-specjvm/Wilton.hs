
{-# LANGUAGE OverloadedStrings #-}

module Wilton ( ) where

import Prelude
    ( IO, Maybe(Nothing)
    , return
    )
import Data.Maybe (isJust)
import Foreign.C.String (CString)
import Foreign.Wilton.FFI (createWiltonError, registerWiltonCall)
import GHC.Generics (Generic)

import Lib (hello)

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    { errHello <- registerWiltonCall "sbs_specjvm_hello" hello
    ; if isJust errHello then createWiltonError errHello

      else createWiltonError Nothing
    }
