
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Lib
    ( run
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.SpecJVM
import SBS.Common.Utils
import SBS.Common.Wilton

run :: SpecJVMInput -> IO ()
run input = do
    putStrLn (showText input)
    obj <- wiltoncall "fs_exists" (object
        [ "path" .= ("fail" :: Text)
        ]) :: IO Object
    let flag = jsonGet obj "exists" :: Bool
    putStrLn (showText flag)

    return ()
