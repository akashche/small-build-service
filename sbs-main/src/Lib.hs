
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( start
    ) where

import Prelude
    ( Either(Left, Right), IO, Maybe(Just, Nothing), Show
    , error, return, show
    )
import Data.Text (Text, pack)
import Data.Text.IO (putStrLn)
import Data.Vector (Vector)
import Foreign.Wilton.FFI (invokeWiltonCall)

import SBS.Common.Data (DyLoadArgs(..), Empty(..))
import SBS.Common.JCStress (MyObjOut)
import SBS.Common.Utils (bytesToString)

start :: Vector Text -> IO Empty
start arguments = do
    putStrLn (pack (show arguments))
    -- load jcstress module
    jcstressErr <- invokeWiltonCall "dyload_shared_library" (DyLoadArgs "sbs_jcstress")
    case jcstressErr of
        Left err -> error (bytesToString err)
        Right (obj :: Maybe Empty) -> return ()
    -- load specjvm module
    specjvmErr <- invokeWiltonCall "dyload_shared_library" (DyLoadArgs "sbs_specjvm")
    case specjvmErr of
        Left err -> error (bytesToString err)
        Right (obj :: Maybe Empty) -> return ()
    -- call specjvm module
    callEither <- invokeWiltonCall "sbs_specjvm_hello" Empty
    case callEither of
        Left err -> error (bytesToString err)
        Right (obj :: Maybe MyObjOut) -> do
            putStrLn (pack (show obj))
            return ()
    return Empty
