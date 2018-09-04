
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Parser
    ( parseJCStressOutput
    ) where

import Prelude ()
import qualified Data.Text.Lazy as TextLazy

import SBS.Common.Prelude
import SBS.Common.Parsec
import Data

oneTest :: Text -> Parser Text
oneTest prefix = do
    skipOne (try (string (unpack prefix)))
    res <- manyTill (alphaNum <|> char '.') (char '\n')
    skipManyTill "\n\n"
    return (pack res)

listOfTests :: Text -> Text -> Parser (Vector Text)
listOfTests header prefix = do
    skipManyTill header
    skipLines 3
    list <- scan
    whitespace
    return (fromList list)
    where
        scan = recur <|> (return [])
        recur = do
            x <- oneTest prefix
            xs <- scan
            return (x:xs)

passedTestsCount :: Parser Int
passedTestsCount = do
    skipManyTill "*** All remaining tests\n"
    skipLines 2
    numStr <- many1 digit
    return (read numStr :: Int)

jcstressResults :: Parser JCStressResults
jcstressResults = do
    tint <- listOfTests "*** INTERESTING tests\n" "[OK]"
    tfail <- listOfTests "*** FAILED tests\n" "[FAILED]"
    terr <- listOfTests "*** ERROR tests\n" "[ERROR]"
    count <- passedTestsCount
    return (JCStressResults count tint tfail terr)

parseJCStressOutput :: TextLazy.Text -> Text -> Either Text JCStressResults
parseJCStressOutput contents path =
    case parse jcstressResults (unpack path) contents of
        Left err -> Left (errToText err)
        Right res -> Right res
