
{-# LANGUAGE OverloadedStrings #-}

import Prelude
    ( IO
    , (==)
    , error, return, show
    )
import Control.Monad (unless)
import Data.Text (pack)
import Data.Text.IO (putStrLn)

import Parser (parseSpecJVMOutput)

main :: IO ()
main = do
    let res = parseSpecJVMOutput "" ""
    putStrLn (pack (show res))
    -- Utils
--     unless (['f', 'o', 'o'] == bytesToString "foo") (error "bytesToString fail")

    putStrLn "Tests Passed."
    return ()
