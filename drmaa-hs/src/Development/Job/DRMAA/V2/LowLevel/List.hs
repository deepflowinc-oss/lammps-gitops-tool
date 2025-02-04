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
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Development.Job.DRMAA.V2.LowLevel.List (
  -- ** Lists
  DRMAAList,
  IsDRMAAListElement (..),
  HasDRMAARep (..),
  createList,
  createListWithoutFinalizer,
  getListSize,
  getListItem,
  addRawListItem,
  addListItem,
  useListAsVector,
  useListAsMaybeVector,
  toDRMAAList,
  fromDRMAAList,
  fromDRMAAListMaybe,

  -- *** Unsafe operators
  unsafeGetListItem,
  freeDRMAAList,
) where

import Control.DeepSeq (NFData, force)
import Control.Exception (bracket, evaluate)
import Control.Monad ((<=<), (>=>))
import qualified Data.ByteString as BS
import Data.Coerce (coerce)
import qualified Data.Vector as V
import Development.Job.DRMAA.V2.LowLevel.String
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs
import Foreign (castPtr)
import Foreign.C
import Foreign.C.HKD
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import GHC.Exts (Proxy#, proxy#)
import GHC.Records (HasField (..))
import qualified Language.C.Inline as C hiding (exp, pure)
import qualified Language.C.Inline.Interruptible as C

C.context $ C.baseCtx <> C.funCtx <> C.bsCtx <> C.fptrCtx <> drmaa2EnumCtx <> drmaa2BasicTypeCtx <> opaqueStructCtx
C.include "<drmaa2.h>"

useListAsVector ::
  forall a b.
  (IsDRMAAListElement a, NFData a) =>
  ReleaseStrategy ->
  DRMAAList (DRMAARep a) ->
  (V.Vector a -> IO b) ->
  IO b
useListAsVector strat list = bracket alloc (const free)
  where
    alloc = do
      sz <- getListSize list
      V.generateM (fromIntegral sz) $ \i ->
        evaluate . force =<< getListItem list (fromIntegral i)
    !list' = coerce list
    free = case strat of
      Free -> [C.exp|void { drmaa2_list_free(& $(drmaa2_list list')) }|]
      NoFree -> pure ()

fromDRMAAListMaybe ::
  (IsDRMAAListElement a, NFData a) =>
  ReleaseStrategy ->
  DRMAAList (DRMAARep a) ->
  IO (Maybe (V.Vector a))
fromDRMAAListMaybe strat list = useListAsMaybeVector strat list pure

fromDRMAAList ::
  (IsDRMAAListElement a, NFData a) =>
  ReleaseStrategy ->
  DRMAAList (DRMAARep a) ->
  IO (V.Vector a)
fromDRMAAList strat list = useListAsVector strat list pure

useListAsMaybeVector ::
  forall a b.
  (IsDRMAAListElement a, NFData a) =>
  ReleaseStrategy ->
  DRMAAList (DRMAARep a) ->
  (Maybe (V.Vector a) -> IO b) ->
  IO b
useListAsMaybeVector strat list = bracket alloc (const free)
  where
    alloc
      | getField @"getDRMAAList" list == nullPtr = pure Nothing
      | otherwise = do
          !sz <- getListSize list
          fmap Just $! V.generateM (fromIntegral sz) $ \i ->
            evaluate . force =<< getListItem list (fromIntegral i)
    !list' = coerce list
    free = case strat of
      Free -> [C.exp|void { drmaa2_list_free(& $(drmaa2_list list')) }|]
      NoFree -> pure ()

getListSize :: DRMAAList a -> IO CLong
getListSize (unsafeCastList -> list) =
  [C.exp|long {drmaa2_list_size($(drmaa2_list list))}|]

unsafeGetListItem :: DRMAAList a -> CLong -> IO (Ptr ())
unsafeGetListItem (unsafeCastList -> list) i =
  [C.exp|const void * {drmaa2_list_get($(drmaa2_list list), $(long i))}|]

unsafeCreateList :: DRMAAListType -> FunPtr (Ptr (Ptr ()) -> IO ()) -> IO (DRMAAList (Ptr ()))
unsafeCreateList listtype callback =
  [C.exp|drmaa2_list {
    drmaa2_list_create(
      $(drmaa2_listtype listtype), 
      $(void (*callback)(void**))
    )
  }|]

getListItem :: (IsDRMAAListElement a) => DRMAAList (DRMAARep a) -> CLong -> IO a
getListItem lst i =
  unsafeFromElementPtr
    . unsafeCastFromPtr
    =<< unsafeGetListItem lst i

createList :: forall a. (IsDRMAAListElement a) => IO (DRMAAList (DRMAARep a))
createList = do
  coerce
    <$> unsafeCreateList
      (listType (proxy# :: Proxy# (DRMAARep a)))
      (coerce $ finalizeDRMAA @(DRMAARep a))

createListWithoutFinalizer :: forall a. (IsDRMAAListElement a) => IO (DRMAAList (DRMAARep a))
createListWithoutFinalizer = do
  coerce
    <$> unsafeCreateList
      (listType (proxy# :: Proxy# (DRMAARep a)))
      [C.funPtr|void nofree(void **a){}|]

unsafeAddListItem :: DRMAAList (Ptr ()) -> Ptr () -> IO Error
unsafeAddListItem lst e =
  [C.exp|drmaa2_error { drmaa2_list_add($(drmaa2_list lst), $(void *e)) }|]

unsafeCastList :: DRMAAList a -> DRMAAList (Ptr ())
unsafeCastList = coerce

addRawListItem :: (HasDRMAARep a) => DRMAAList a -> a -> IO Error
addRawListItem lst =
  unsafeAddListItem (unsafeCastList lst) . unsafeCastToPtr

addListItem :: (IsDRMAAListElement a) => DRMAAList (DRMAARep a) -> a -> IO Error
addListItem lst =
  unsafeAddListItem (unsafeCastList lst)
    . unsafeCastToPtr
    <=< unsafeToElementPtr

toDRMAAList :: (IsDRMAAListElement a) => V.Vector a -> IO (DRMAAList (DRMAARep a))
toDRMAAList lst = do
  l <- createList
  V.mapM_
    ( addListItem l >=> \case
        Success -> pure ()
        _ -> error "List construction failed"
    )
    lst
  pure l

freeDRMAAList :: FunPtr (Ptr (DRMAAList a) -> IO ())
freeDRMAAList =
  coerce
    [C.funPtr|void free_l(drmaa2_list* l) {  drmaa2_list_free(l);  }|]

instance (IsDRMAAListElement a, NFData a) => Packable (V.Vector a) where
  type Unpacked_ (V.Vector a) = DRMAAList (DRMAARep a)
  packM = fromDRMAAListMaybe NoFree

  unpackM Nothing = pure $ DRMAAList nullPtr
  unpackM (Just v) = toDRMAAList v

class HasDRMAARep a where
  unsafeCastFromPtr :: Ptr () -> a
  unsafeCastToPtr :: a -> Ptr ()
  finalizeDRMAA :: FunPtr (Ptr a -> IO ())
  listType :: Proxy# a -> DRMAAListType

class (HasDRMAARep (DRMAARep a)) => IsDRMAAListElement a where
  type DRMAARep a = rep | rep -> a
  type DRMAARep a = a
  unsafeFromElementPtr :: DRMAARep a -> IO a
  default unsafeFromElementPtr :: (a ~ DRMAARep a) => DRMAARep a -> IO a
  unsafeFromElementPtr = pure
  unsafeToElementPtr :: a -> IO (DRMAARep a)
  default unsafeToElementPtr :: (a ~ DRMAARep a) => a -> IO (DRMAARep a)
  unsafeToElementPtr = pure

instance IsDRMAAListElement BS.ByteString where
  type DRMAARep BS.ByteString = DRMAAString
  unsafeFromElementPtr = fromDRMAAString NoFree
  {-# INLINE unsafeFromElementPtr #-}
  unsafeToElementPtr = toDRMAAString

instance HasDRMAARep DRMAAString where
  finalizeDRMAA = freeDRMAAString
  listType _ = StringList
  unsafeCastFromPtr = DRMAAString . castPtr
  unsafeCastToPtr = castPtr . getField @"getDRMAAString"

instance IsDRMAAListElement Job

instance HasDRMAARep Job where
  unsafeCastFromPtr = Job . castPtr
  unsafeCastToPtr = castPtr . getField @"getJob"
  finalizeDRMAA = [C.funPtr|void free_j(drmaa2_j *job) {  drmaa2_j_free(job);  }|]
  listType _ = JobList
