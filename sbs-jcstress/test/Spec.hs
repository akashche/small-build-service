
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Utils

import Data
import Lib
import Parser

parseLog :: Text -> IO JCStressResults
parseLog path =
    withFileText path fun
    where
        fun tx = return (parseJCStressOutput tx path)

main :: IO ()
main = do
    baseline <- parseLog "test/jcstress_abridged.log"
    putStrLn (showText baseline)
    res <- parseLog "test/jcstress_abridged_alt.log"
    let diff = diffResults baseline res
    putStrLn (showText diff)

    putStrLn "Tests Passed."
    return ()
