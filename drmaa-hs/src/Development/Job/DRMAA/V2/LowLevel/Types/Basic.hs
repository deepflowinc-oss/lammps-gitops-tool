{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Job.DRMAA.V2.LowLevel.Types.Basic (
  DRMAAString (..),
  DRMAAList (..),
  DRMAADict (..),
  OpaqueExt (..),
  OpaqueExtPtr (..),
  drmaa2BasicTypeCtx,
  drmaa2BasicTypeTable,
) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import Foreign
import Foreign.C
import Foreign.C.HKD (Packable (..))
import GHC.Generics (Generic)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

newtype DRMAAString = DRMAAString {getDRMAAString :: CString}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, Storable)

newtype DRMAAList a = DRMAAList {getDRMAAList :: Ptr (DRMAAList a)}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, Storable)

newtype DRMAADict = DRMAADict {getDRMAADict :: Ptr DRMAADict}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, Storable)

data OpaqueExt = OpaqueExt
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)

newtype OpaqueExtPtr = OpaqueExtPtr {unOpaqueExt :: Ptr ()}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (NFData, Storable)

instance Packable OpaqueExt where
  type Unpacked_ OpaqueExt = OpaqueExtPtr
  packM = const $ pure Nothing
  unpackM = const $ pure $ OpaqueExtPtr nullPtr

drmaa2BasicTypeCtx :: C.Context
drmaa2BasicTypeCtx = mempty {C.ctxTypesTable = drmaa2BasicTypeTable}

drmaa2BasicTypeTable :: C.TypesTable
drmaa2BasicTypeTable =
  Map.fromList
    [ (C.TypeName "drmaa2_string", [t|DRMAAString|])
    , (C.TypeName "drmaa2_list", [t|DRMAAList (Ptr ())|])
    , (C.TypeName "drmaa2_string_list", [t|DRMAAList DRMAAString|])
    , (C.TypeName "drmaa2_dict", [t|DRMAADict|])
    ]
