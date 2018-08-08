
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Utils

import Parser

main :: IO ()
main = do
    withFileText "test/specjvm.log" (\tx -> do
        let resEither = parseSpecJVMOutput tx "test.log"
        case resEither of
            Left err -> putStrLn err
            Right res -> putStrLn (pack (show res))
        return () )

    putStrLn "Tests Passed."
    return ()
