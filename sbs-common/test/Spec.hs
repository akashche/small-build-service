
{-# LANGUAGE OverloadedStrings #-}

import Prelude
    ( IO, Either(Left)
    , (.), (==)
    , error, return, show
    )
import Control.Monad (unless)
import Data.Text (Text, pack, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.IO (putStrLn)

import SBS.Common.Data (Empty(..))
import SBS.Common.Utils (bytesToString, encodeJsonToText)

main :: IO ()
main = do
    -- Data
    unless ("{}" == encodeJsonToText Empty) (error "empty fail")

    -- Utils
    unless (['f', 'o', 'o'] == bytesToString "foo") (error "bytesToString fail")

    putStrLn "Tests Passed."
    return ()
