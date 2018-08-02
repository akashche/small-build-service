
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

import Lib (start)

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    { errStart <- registerWiltonCall "sbs_start" start
    ; if isJust errStart then createWiltonError errStart

      else createWiltonError Nothing
    }
