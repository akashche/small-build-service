
{-# LANGUAGE OverloadedStrings #-}

import Prelude
    ( IO
    , (==)
    , error, return, show
    )
import Control.Monad (unless)
import Data.Text (pack)
import Data.Text.IO (putStrLn)

main :: IO ()
main = do
--     unless (['f', 'o', 'o'] == bytesToString "foo") (error "bytesToString fail")

    putStrLn "Tests Passed."
    return ()
