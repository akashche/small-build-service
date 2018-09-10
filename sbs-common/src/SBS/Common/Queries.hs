
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Queries
    ( Queries
    , loadQueries
    ) where

import Prelude ()
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Text as Text

import SBS.Common.Prelude
import SBS.Common.Parsec
import SBS.Common.Utils

type Queries = HashMap Text Text

singleQuery :: Parser (Text, Text)
singleQuery = do
    skipOne (string "/**")
    name <- many1 alphaNum
    whitespace
    skipOne (string "*/")
    value <- manyTill anyChar
        (   (try (lookAhead (string "/**")) >> return ())
        <|> eof
        )
    return ((convert name), (convert value))
    where
        convert st = Text.dropWhileEnd isEol (pack st)
        isEol ch = '\n' == ch

queries :: Parser Queries
queries = do
    skipManyTill "/**"
    li <- many1 singleQuery
    return (HashMap.fromList li)

loadQueries :: Text -> IO Queries
loadQueries path =
    withFileText path fun
    where
        fun contents =
            case parse queries (unpack path) contents of
                Left err -> errorText (errToText err)
                Right res -> return res
