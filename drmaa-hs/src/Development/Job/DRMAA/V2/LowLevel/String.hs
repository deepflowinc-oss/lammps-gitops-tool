{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Development.Job.DRMAA.V2.LowLevel.String (
  DRMAAString (),
  fromDRMAAString,
  fromDRMAAStringMaybe,
  toDRMAAString,
  freeDRMAAString,
) where

import Control.DeepSeq (force)
import Control.Exception (evaluate, finally)
import qualified Data.ByteString as BS
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Development.Job.DRMAA.V2.LowLevel.Types.Misc
import Foreign (nullPtr)
import Foreign.C.HKD
import Foreign.Ptr (FunPtr, Ptr)
import qualified Language.C.Inline as C hiding (exp, pure)
import qualified Language.C.Inline.Interruptible as C

C.context (C.baseCtx <> C.funCtx <> C.bsCtx <> C.fptrCtx <> drmaa2EnumCtx <> drmaa2BasicTypeCtx)
C.include "<drmaa2.h>"
C.include "<string.h>"

fromDRMAAString :: ReleaseStrategy -> DRMAAString -> IO BS.ByteString
fromDRMAAString strat str@(DRMAAString cstr) =
  case strat of
    Free -> body `finally` [C.exp|void { drmaa2_string_free(& $(drmaa2_string str)) }|]
    NoFree -> body
  where
    body = evaluate . force =<< BS.packCString cstr
    {-# INLINE body #-}

fromDRMAAStringMaybe :: ReleaseStrategy -> DRMAAString -> IO (Maybe BS.ByteString)
fromDRMAAStringMaybe strat str@(DRMAAString cstr)
  | cstr == nullPtr = pure Nothing
  | otherwise = Just <$> fromDRMAAString strat str

toDRMAAString :: BS.ByteString -> IO DRMAAString
toDRMAAString bs =
  DRMAAString <$> [C.exp|char * {strndup($bs-cstr:bs, $bs-len:bs)} |]

freeDRMAAString :: FunPtr (Ptr DRMAAString -> IO ())
freeDRMAAString =
  [C.funPtr|void free_l(drmaa2_string* l) { drmaa2_string_free(l); }|]

instance Packable BS.ByteString where
  type Unpacked_ BS.ByteString = DRMAAString
  packM = fromDRMAAStringMaybe NoFree
  unpackM = maybe (pure $ DRMAAString nullPtr) toDRMAAString
