{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs (
  JobSessionStruct,
  JobSession (..),
  JobStruct,
  Job (..),
  MonitorSessionStruct,
  MonitorSession (..),
  opaqueStructTable,
  opaqueStructCtx,
  JobArrayStruct,
  JobArray (..),
) where

import Control.DeepSeq (NFData)
import qualified Data.Map.Strict as Map
import Foreign
import GHC.Generics (Generic)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

data JobSessionStruct
  deriving (Show, Eq, Ord, Generic)

newtype JobSession = JobSession {getJobSession :: Ptr JobSessionStruct}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Storable, NFData)

data JobStruct
  deriving (Show, Eq, Ord, Generic)

newtype Job = Job {getJob :: Ptr JobStruct}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Storable, NFData)

data JobArrayStruct

newtype JobArray = JobArray {getJob :: Ptr JobArrayStruct}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Storable, NFData)

data MonitorSessionStruct
  deriving (Show, Eq, Ord, Generic)

newtype MonitorSession = MonitorSession {getJob :: Ptr MonitorSessionStruct}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Storable, NFData)

opaqueStructCtx :: C.Context
opaqueStructCtx = mempty {C.ctxTypesTable = opaqueStructTable}

opaqueStructTable :: C.TypesTable
opaqueStructTable =
  Map.fromList
    [ (C.Struct "drmaa2_j_s", [t|JobStruct|])
    , (C.TypeName "drmaa2_j", [t|Job|])
    , (C.TypeName "drmaa2_jsession", [t|JobSession|])
    , (C.Struct "drmaa2_jsession_s", [t|JobSessionStruct|])
    , (C.TypeName "drmaa2_msession", [t|MonitorSession|])
    , (C.Struct "drmaa2_msession_s", [t|MonitorSessionStruct|])
    , (C.Struct "drmaa2_jarray_s", [t|JobArrayStruct|])
    , (C.Struct "drmaa2_jarray", [t|JobArray|])
    ]
