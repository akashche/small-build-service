
{-# LANGUAGE DeriveDataTypeable #-}
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
    --unless (['f', 'o', 'o'] == bytesToString "foo") (error "bytesToString fail")

    putStrLn "Tests Passed."
    return ()
