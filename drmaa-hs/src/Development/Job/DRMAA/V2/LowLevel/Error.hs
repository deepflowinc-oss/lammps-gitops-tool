{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fobject-code #-}

module Development.Job.DRMAA.V2.LowLevel.Error (
  Error (..),
  lastError,
  lastErrorText,
  DRMAAException (..),
  getLastException,
  maybeToDRMAException,
  maybeToDRMAExceptionThrow,
  throwOnError,
  reifyOnError,
) where

import Control.Exception (Exception, throwIO)
import Control.Exception.Safe (Exception (..))
import Control.Monad ((<=<))
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.Job.DRMAA.V2.LowLevel.String (fromDRMAAStringMaybe)
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import GHC.Exception (prettyCallStackLines)
import GHC.Generics (Generic)
import GHC.Stack (CallStack, HasCallStack, callStack)
import qualified Language.C.Inline as C hiding (exp, pure)
import qualified Language.C.Inline.Interruptible as C

C.context $ C.baseCtx <> drmaa2EnumCtx <> drmaa2BasicTypeCtx
C.include "<drmaa2.h>"

{- |
Returns the last DRMAA2 related error happend within the thread the function has called.
Internally thread local storage is used by DRMAA2 functions for storing errors.
This function is usually called when a previous DRMAA2 function call failed resulting in returning a NULL value.
A detailed description of the error can be get by using the 'lastErrorText'().

Returns a 'Error' value of the last error occurred.
If no error happend yet 'Success' is returned.
-}
lastError :: IO Error
lastError = [C.exp|drmaa2_error {drmaa2_lasterror()}|]

{- |
Returns the last DRMAA2 related error happend within the thread the function has called.
Internally thread local storage is used by DRMAA2 functions for storing errors.
This function is usually called when a previous DRMAA2 function call failed resulting in returning a NULL value.
A numerical description of the error can be get by using the 'lastError' call.

Returns an unspecified string value describing the last error occurred.
If no error happend yet the empty string is returned.
-}
lastErrorText :: IO (Maybe BS.ByteString)
lastErrorText =
  fromDRMAAStringMaybe Free
    =<< [C.exp|drmaa2_string {drmaa2_lasterror_text()}|]

data DRMAAException
  = DRMAAException !CallStack !Error !BS.ByteString
  | DRMAAExceptionNoCode !CallStack !BS.ByteString
  | CouldNotDetermineJobState !CallStack !BS.ByteString
  | ErrorInfoLost !CallStack
  deriving (Show, Generic)

instance Exception DRMAAException where
  displayException (DRMAAException cstack errCode msg) =
    "DRMAA Exception (code: "
      <> show errCode
      <> ", "
      <> show (fromEnum errCode)
      <> "): "
      <> T.unpack (T.decodeUtf8 msg)
      <> unlines (map ("  " <>) $ prettyCallStackLines cstack)
  displayException (DRMAAExceptionNoCode cstack msg) =
    "DRMAA Exception (Unknown Error): "
      <> T.unpack (T.decodeUtf8 msg)
      <> unlines (map ("  " <>) $ prettyCallStackLines cstack)
  displayException (CouldNotDetermineJobState cstack msg) =
    "DRNAAException - Could not deterimine job stat: "
      <> T.unpack (T.decodeUtf8 msg)
      <> unlines (map ("  " <>) $ prettyCallStackLines cstack)
  displayException (ErrorInfoLost cstack) =
    "DRNAAException: Unknown error occurred (info lost)"
      <> unlines (map ("  " <>) $ prettyCallStackLines cstack)

getLastException :: (HasCallStack) => IO (Maybe DRMAAException)
getLastException = do
  code <- lastError
  case code of
    Success -> fmap (DRMAAExceptionNoCode callStack) <$> lastErrorText
    ec -> Just . DRMAAException callStack ec . fromMaybe "<NO MSG>" <$> lastErrorText

maybeToDRMAException :: (HasCallStack) => Maybe a -> IO (Either DRMAAException a)
maybeToDRMAException =
  maybe (Left . fromMaybe (ErrorInfoLost callStack) <$> getLastException) (pure . Right)

maybeToDRMAExceptionThrow :: (HasCallStack) => Maybe a -> IO a
maybeToDRMAExceptionThrow = either throwIO pure <=< maybeToDRMAException

throwOnError :: Error -> IO ()
throwOnError = mapM_ throwIO <=< reifyOnError

reifyOnError :: (HasCallStack) => Error -> IO (Maybe DRMAAException)
reifyOnError Success = pure Nothing
reifyOnError ec =
  Just . DRMAAException callStack ec . fromMaybe "<NO MSG>" <$> lastErrorText
