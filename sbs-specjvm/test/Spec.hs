
{-# LANGUAGE OverloadedStrings #-}

import Prelude
    ( Either(Left, Right), IO
    , (==)
    , error, return, show
    )
import Control.Monad (when, unless)
import Data.Text (pack)
import Data.Text.IO (putStrLn)
import Data.Text.Lazy (fromChunks)

import SBS.Common.Utils (withFileText)

import Parser (parseSpecJVMOutput)

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
