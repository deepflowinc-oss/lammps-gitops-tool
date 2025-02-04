{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}

module Development.Job.DRMAA.V2.Managed (
  Freeable (..),
  Managed (..),
  toManaged,
  unsafeToManaged,
  withNullableUnpackedMoved,
  withNullableUnpackedManaged,
  withUnpackedMoved,
  withUnpackedManaged,
  unsafeUnmanaged,
) where

import Barbies
import Control.DeepSeq
import Control.Monad ((<=<))
import Data.Coerce (Coercible, coerce)
import qualified Data.Map.Strict as Map
import Development.Job.DRMAA.V2.LowLevel.List (freeDRMAAList)
import Development.Job.DRMAA.V2.LowLevel.String (freeDRMAAString)
import Development.Job.DRMAA.V2.LowLevel.Types
import Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs (JobArrayStruct, opaqueStructCtx)
import Foreign (FunPtr, Ptr, Storable, newForeignPtr, newForeignPtr_, nullPtr, poke, with)
import Foreign.C.HKD
import Foreign.ForeignPtr (ForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import GHC.Generics (Generic)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

C.context
  $ C.baseCtx
  <> opaqueStructCtx
  <> mempty
    { C.ctxTypesTable =
        Map.fromList
          [ (C.TypeName "drmaa2_jtemplate_s", [t|JobTemplateStruct|])
          , (C.TypeName "drmaa2_jinfo_s", [t|JobInfoStruct|])
          ]
    }
C.include "<drmaa2.h>"

class Freeable a where
  freeStructPtr :: FunPtr (Ptr a -> IO ())

instance Freeable DRMAAString where
  freeStructPtr = freeDRMAAString

instance Freeable (DRMAAList a) where
  freeStructPtr = freeDRMAAList

instance Freeable JobStruct where
  freeStructPtr = [C.funPtr|void freeJS(struct drmaa2_j_s *j) { drmaa2_j_free(&j); }|]

instance Freeable JobSessionStruct where
  freeStructPtr = [C.funPtr|void freeJS(struct drmaa2_jsession_s *j) { drmaa2_jsession_free(&j); }|]

instance Freeable JobInfoStruct where
  -- FIXME: Contrary to the spec, it seems UGE sometimes returns the SAME pointers
  -- multiple times. So we would rather stop freeing it, hoping that this won't lead
  -- to leak...
  freeStructPtr = [C.funPtr|void freeJI(drmaa2_jinfo_s *j) { /* drmaa2_jinfo_free(&j); */ }|]

instance Freeable JobTemplateStruct where
  freeStructPtr = [C.funPtr|void freeJI(drmaa2_jtemplate_s *j) { drmaa2_jtemplate_free(&j); }|]

instance Freeable MonitorSessionStruct where
  freeStructPtr = [C.funPtr|void freeJI(struct drmaa2_msession_s *j) { drmaa2_msession_free(&j); }|]

instance Freeable JobArrayStruct where
  freeStructPtr = [C.funPtr|void freeJI(struct drmaa2_jarray_s *j) { drmaa2_jarray_free(&j); }|]

-- | Wrapper for a pointer managed on DRAMAA side.
newtype Managed struct = Managed {runManaged :: ForeignPtr struct}
  deriving (Show, Eq, Ord, Generic)

instance NFData (Managed s) where
  rnf = rnf . unsafeForeignPtrToPtr . runManaged

toManaged ::
  forall raw struct.
  (Coercible raw (Ptr struct), Freeable struct) =>
  raw ->
  IO (Maybe (Managed struct))
toManaged raw = do
  let ptr = coerce @_ @(Ptr struct) raw
  if ptr == nullPtr
    then pure Nothing
    else Just <$> unsafeToManaged ptr

withUnpackedMoved ::
  ( TraversableB struct
  , ConstraintsB struct
  , Storable (struct Unpacked)
  , AllB Packable struct
  ) =>
  IO (Ptr (struct Unpacked)) ->
  struct Maybe ->
  (Managed (struct Unpacked) -> IO a) ->
  IO a
withUnpackedMoved alloc struct k = do
  ptr <- alloc
  poke ptr =<< btraverseC @Packable (fmap Unpacked . unpackM) struct
  k . Managed =<< newForeignPtr_ ptr

unsafeUnmanaged :: Ptr a -> IO (Maybe (Managed a))
unsafeUnmanaged ptr
  | ptr == nullPtr = pure Nothing
  | otherwise = Just . Managed <$> newForeignPtr_ ptr

withNullableUnpackedMoved ::
  ( TraversableB struct
  , ConstraintsB struct
  , Storable (struct Unpacked)
  , AllB Packable struct
  ) =>
  IO (Ptr (struct Unpacked)) ->
  Maybe (struct Maybe) ->
  (Managed (struct Unpacked) -> IO a) ->
  IO a
withNullableUnpackedMoved _ Nothing k =
  k . Managed =<< newForeignPtr_ nullPtr
withNullableUnpackedMoved alloc (Just struct) k = do
  withUnpackedMoved alloc struct k

withUnpackedManaged ::
  ( TraversableB struct
  , ConstraintsB struct
  , Storable (struct Unpacked)
  , Freeable (struct Unpacked)
  , AllB Packable struct
  ) =>
  struct Maybe ->
  (Managed (struct Unpacked) -> IO a) ->
  IO a
withUnpackedManaged struct k = do
  unpacked <- btraverseC @Packable (fmap Unpacked . unpackM) struct
  with unpacked (k <=< unsafeToManaged)

withNullableUnpackedManaged ::
  ( TraversableB struct
  , ConstraintsB struct
  , Storable (struct Unpacked)
  , Freeable (struct Unpacked)
  , AllB Packable struct
  ) =>
  Maybe (struct Maybe) ->
  (Managed (struct Unpacked) -> IO a) ->
  IO a
withNullableUnpackedManaged Nothing k = k . Managed =<< newForeignPtr_ nullPtr
withNullableUnpackedManaged (Just struct) k = withUnpackedManaged struct k

-- | Similar to 'toManaged', but without checking NULL
unsafeToManaged :: (Freeable a) => Ptr a -> IO (Managed a)
unsafeToManaged ptr = Managed <$> newForeignPtr freeStructPtr ptr
