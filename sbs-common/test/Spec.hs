
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.Utils

main :: IO ()
main = do
    -- Data
    unless ("{}" == encodeJsonText Empty) (errorText "empty fail")

    -- Utils
    unless ("foo" == showText ("foo" :: Text)) (errorText "showText fail")
    unless ("foo" == showText ("foo" :: String)) (errorText "showText fail")
    unless ("foo" == showText ("foo" :: ByteString)) (errorText "showText fail")

    putStrLn "Tests Passed."
    return ()
