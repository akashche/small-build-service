
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Utils

import Data
import Diff
import Parser

parseLog :: Text -> IO SpecJVMResults
parseLog path =
    withFileText path fun
    where
        fun tx = return (parseSpecJVMOutput tx path)

main :: IO ()
main = do
    baseline <- parseLog "test/specjvm.log"
    res <- parseLog "test/specjvm_alt.log"
    let diff = diffResults baseline res
    putStrLn (showText diff)

    putStrLn "Tests Passed."
    return ()
