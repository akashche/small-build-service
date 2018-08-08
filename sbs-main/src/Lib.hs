
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Lib
    ( start
    ) where

import Prelude ()

import SBS.Common.Prelude
import SBS.Common.Data
import SBS.Common.JCStress
import SBS.Common.Utils

start :: Vector Text -> IO Empty
start arguments = do
    putStrLn (pack (show arguments))
    -- load jcstress module
    jcstressErr <- invokeWiltonCall "dyload_shared_library" (DyLoadArgs "sbs_jcstress")
    case jcstressErr of
        Left err -> error (bytesToString err)
        Right (_ :: Maybe Empty) -> return ()
    -- load specjvm module
    specjvmErr <- invokeWiltonCall "dyload_shared_library" (DyLoadArgs "sbs_specjvm")
    case specjvmErr of
        Left err -> error (bytesToString err)
        Right (_ :: Maybe Empty) -> return ()
    -- call specjvm module
    callEither <- invokeWiltonCall "sbs_specjvm_hello" Empty
    case callEither of
        Left err -> error (bytesToString err)
        Right (obj :: Maybe MyObjOut) -> do
            putStrLn (pack (show obj))
            return ()
    return Empty
