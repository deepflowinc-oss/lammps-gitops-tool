{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Development.Job.DRMAA.V2.LowLevel.ImplementationSpecific (
  WithImplSpec (..),
  withUnpackedWithImplSpecMoved,
  bpackStructWithImplSpec,
  setInstanceValue,
  getInstanceValue,
  jobInfoImplSpec,
  jobTemplateImplSpec,
  machineInfoImplSpec,
  notificationImplSpec,
  queueInfoImplSpec,
  reservationInfoImplSpec,
  reservationTemplateImplSpec,
  withNullableUnpackedWithImplSpecMoved,
) where

import Barbies
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad (forM, forM_, join, (<$!>))
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), withObject, (.:?))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as AKM
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Kind (Constraint)
import Data.Semigroup.Generic
import qualified Data.Vector as V
import Development.Job.DRMAA.V2.LowLevel.Error
import Development.Job.DRMAA.V2.LowLevel.ImplementationSpecific.TH (defineImplSpecFunc)
import Development.Job.DRMAA.V2.LowLevel.List (fromDRMAAListMaybe)
import Development.Job.DRMAA.V2.LowLevel.String (fromDRMAAStringMaybe)
import Development.Job.DRMAA.V2.LowLevel.Types
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import Development.Job.DRMAA.V2.Managed
import Foreign (Ptr, Storable, withForeignPtr)
import Foreign.C
import Foreign.C.HKD
import Foreign.Ptr (castPtr)
import GHC.Exts (Proxy#, proxy#)
import GHC.Generics (Generic)
import qualified Language.C.Inline as C hiding (exp, pure)
import qualified Language.C.Inline.Interruptible as C

-- N.B.: HLS won't be happy when we use 'drmaa2Ctx' instead,
-- although GHC itself is happy with it...
C.context $ C.baseCtx <> C.bsCtx <> drmaa2BasicTypeCtx <> drmaa2EnumCtx
C.include "<drmaa2.h>"

data WithImplSpec struct f = WithImplSpec
  { payload :: !(struct f)
  , implSpec :: !(Maybe (HM.HashMap BS.ByteString BS.ByteString))
  }
  deriving (Generic, Show, Eq, Ord)
  deriving anyclass (FunctorB, TraversableB, ConstraintsB, ApplicativeB)
  deriving (Semigroup, Monoid) via GenericSemigroupMonoid (WithImplSpec struct f)
  deriving anyclass (NFData, Hashable)

instance (FromJSON (struct f)) => FromJSON (WithImplSpec struct f) where
  parseJSON obj = do
    payload <- parseJSON obj
    implSpec <-
      withObject
        "object"
        (.:? implSpecKey)
        obj
        <&> fmap (HM.fromList . map (Bi.bimap BS8.pack BS8.pack) . HM.toList)
    pure WithImplSpec {..}

implSpecKey :: A.Key
implSpecKey = "implementationSpecific"

instance (ToJSON (struct f)) => ToJSON (WithImplSpec struct f) where
  toJSON WithImplSpec {..} =
    case toJSON payload of
      Object dic ->
        A.Object
          $ maybe
            id
            ( AKM.insert
                implSpecKey
                . toJSON
                . HM.fromList
                . map (Bi.bimap BS8.unpack BS8.unpack)
                . HM.toList
            )
            implSpec
            dic
      _ -> error "WithImplSpec: Invalid ToJSON instance for payload"

withUnpackedWithImplSpecMoved ::
  ( AllB Packable struct
  , TraversableB struct
  , ConstraintsB struct
  , Storable (struct Unpacked)
  ) =>
  IO (Ptr (struct Unpacked)) ->
  WithImplSpec struct Maybe ->
  (Managed (struct Unpacked) -> IO a) ->
  IO a
withUnpackedWithImplSpecMoved alloc WithImplSpec {..} act =
  withUnpackedMoved alloc payload $ \man -> do
    forM_ implSpec $ \impl -> withForeignPtr (runManaged man) $ \ptr ->
      forM_ (HM.toList impl) \(k, v) ->
        throwOnError =<< setInstanceValue ptr k v
    act man

withNullableUnpackedWithImplSpecMoved ::
  ( AllB Packable struct
  , TraversableB struct
  , ConstraintsB struct
  , Storable (struct Unpacked)
  ) =>
  IO (Ptr (struct Unpacked)) ->
  Maybe (WithImplSpec struct Maybe) ->
  (Managed (struct Unpacked) -> IO a) ->
  IO a
withNullableUnpackedWithImplSpecMoved alloc mImplSpec act =
  withNullableUnpackedMoved alloc (payload <$> mImplSpec) $ \man -> do
    forM_ (implSpec =<< mImplSpec) $ \spc ->
      withForeignPtr (runManaged man) $ \ptr ->
        spc & HM.toList & mapM_ \(k, v) ->
          throwOnError =<< setInstanceValue ptr k v
    act man

type HasImplSpec :: k -> Constraint
class HasImplSpec struct where
  getImplSpec# :: Proxy# struct -> Maybe (V.Vector BS.ByteString)

bpackStructWithImplSpec ::
  forall struct.
  ( TraversableB struct
  , ConstraintsB struct
  , HasImplSpec struct
  , Storable (struct Unpacked)
  , AllB Packable struct
  , NFData (struct Maybe)
  ) =>
  Ptr (struct Unpacked) ->
  IO (WithImplSpec struct Maybe)
bpackStructWithImplSpec ptr = do
  let !keys = getImplSpec# (proxy# :: Proxy# struct)
  !implSpec <-
    evaluate
      . force
      =<< forM
        keys
        ( fmap (HM.fromList . V.toList . V.mapMaybe sequenceA) . mapM \ !k ->
            (k,) <$!> (evaluate . force =<< getInstanceValue ptr k)
        )
  !payload <- bpackStruct ptr
  pure WithImplSpec {..}

asVoidPtr :: Ptr a -> Ptr ()
asVoidPtr = castPtr

setInstanceValue :: Ptr a -> BS.ByteString -> BS.ByteString -> IO Error
setInstanceValue (asVoidPtr -> inst) name val =
  [C.exp| drmaa2_error {
     drmaa2_set_instance_value($(void* inst), $bs-cstr:name, $bs-cstr:val)
  }|]

getInstanceValue :: Ptr a -> BS.ByteString -> IO (Maybe BS.ByteString)
getInstanceValue (asVoidPtr -> inst) name =
  fromDRMAAStringMaybe Free
    =<< [C.exp|drmaa2_string {
          drmaa2_get_instance_value($(void *inst), $bs-cstr:name)
        }|]

defineImplSpecFunc
  "jobInfoImplSpec"
  "drmaa2_string_list { drmaa2_jinfo_impl_spec() }"

defineImplSpecFunc
  "jobTemplateImplSpec"
  "drmaa2_string_list { drmaa2_jtemplate_impl_spec() }"

defineImplSpecFunc
  "machineInfoImplSpec"
  "drmaa2_string_list { drmaa2_machineinfo_impl_spec() }"

defineImplSpecFunc
  "notificationImplSpec"
  "drmaa2_string_list { drmaa2_notification_impl_spec() }"

defineImplSpecFunc
  "queueInfoImplSpec"
  "drmaa2_string_list { drmaa2_queueinfo_impl_spec() }"

defineImplSpecFunc
  "reservationInfoImplSpec"
  "drmaa2_string_list { drmaa2_rinfo_impl_spec() }"

defineImplSpecFunc
  "reservationTemplateImplSpec"
  "drmaa2_string_list { drmaa2_rtemplate_impl_spec() }"

instance HasImplSpec JobTemplateF where
  getImplSpec# _ = jobTemplateImplSpec

instance HasImplSpec JobInfoF where
  getImplSpec# _ = jobInfoImplSpec
