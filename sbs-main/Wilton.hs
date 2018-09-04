
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Wilton ( ) where

import Prelude ()

import SBS.Common.Prelude
import Lib

foreign export ccall wilton_module_init :: IO CString
wilton_module_init :: IO CString
wilton_module_init = do
    { errStart <- registerWiltonCall "sbs_start" start
    ; if isJust errStart then createWiltonError errStart

      else createWiltonError Nothing
    }
