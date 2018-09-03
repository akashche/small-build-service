
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module SBS.Common.Wilton.Call
    ( wiltoncall
    , wiltoncallText
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Utils

-- wilton access with stack unwinding

wiltoncall ::
    forall arguments result . (ToJSON arguments, Data result, FromJSON result) =>
    Text -> arguments -> IO result
wiltoncall callName args = do
    err <- invokeWiltonCall (encodeUtf8 callName) args
    case err of
        Left msg -> errorText (decodeUtf8 msg)
        Right (resMaybe :: Maybe result) ->
            case resMaybe of
                Nothing -> return (undefined :: result)
                Just res -> return res

wiltoncallText :: Text -> Text -> IO Text
wiltoncallText callName args = do
    err <- invokeWiltonCallByteString (encodeUtf8 callName) (encodeUtf8 args)
    case err of
        Left msg -> errorText (decodeUtf8 msg)
        Right res -> return (decodeUtf8 res)

