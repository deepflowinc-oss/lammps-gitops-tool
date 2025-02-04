{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
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
{-# OPTIONS_GHC -fplugin=Foreign.Storable.Generic.Plugin #-}

module Development.Job.DRMAA.V2.LowLevel.Types (
  drmaa2StructTypeTable,
  drmaa2StructCtx,
  JobSession (..),
  JobSessionStruct,
  Job (..),
  JobStruct,
  MonitorSession (..),
  MonitorSessionStruct,
  JobInfo (..),
  JobInfoStruct,
  JobInfoF (..),
  JobTemplateStruct,
  JobTemplateF (..),
  JobTemplate (..),
  DRMAAListEntryFree,

  -- * Basic data-types
  module Development.Job.DRMAA.V2.LowLevel.Types.Basic,

  -- * Enums
  module Development.Job.DRMAA.V2.LowLevel.Types.Enums,
) where

import Barbies
import Control.DeepSeq (NFData)
import qualified Data.Aeson as J
import qualified Data.ByteString as BS
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Semigroup.Generic
import Data.Time (NominalDiffTime)
import Data.Time.Clock (UTCTime)
import qualified Data.Vector as V
import Development.Job.DRMAA.V2.LowLevel.Dict
import Development.Job.DRMAA.V2.LowLevel.List
import Development.Job.DRMAA.V2.LowLevel.String
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs
import Foreign
import Foreign.C.HKD
import Foreign.Storable.Generic (GStorable)
import GHC.Generics (Generic)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C

type DRMAAListEntryFree = FunPtr (Ptr (Ptr ()) -> IO ())

type JobInfoStruct = JobInfoF Unpacked

data JobInfoF f = JobInfo
  { jobId :: !(f BS.ByteString)
  , jobName :: !(f BS.ByteString)
  , exitStatus :: !(f Int)
  , terminatingSignal :: !(f BS.ByteString)
  , annotation :: !(f BS.ByteString)
  , jobState :: !(f JobState)
  , jobSubState :: !(f BS.ByteString)
  , allocatedMachines :: !(f (V.Vector BS.ByteString))
  , submissionMachine :: !(f BS.ByteString)
  , jobOwner :: !(f BS.ByteString)
  , slots :: !(f Int64)
  , queueName :: !(f BS.ByteString)
  , wallclockTime :: !(f NominalDiffTime)
  , cpuTime :: !(f Int64)
  , submissionTime :: !(f UTCTime)
  , dispatchTime :: !(f UTCTime)
  , finishTime :: !(f UTCTime)
  }
  deriving (Generic)
  deriving anyclass (FunctorB, ConstraintsB, TraversableB, ApplicativeB)

deriving instance (AllBF Show f JobInfoF) => Show (JobInfoF f)

deriving instance (AllBF Eq f JobInfoF) => Eq (JobInfoF f)

deriving instance (AllBF Ord f JobInfoF) => Ord (JobInfoF f)

deriving anyclass instance
  (AllBF NFData f JobInfoF) =>
  NFData (JobInfoF f)

deriving anyclass instance
  (AllBF Hashable f JobInfoF) =>
  Hashable (JobInfoF f)

deriving anyclass instance
  (AllBF J.FromJSON f JobInfoF) =>
  J.FromJSON (JobInfoF f)

deriving anyclass instance
  (AllBF J.ToJSON f JobInfoF) =>
  J.ToJSON (JobInfoF f)

deriving via
  GenericSemigroupMonoid (JobInfoF f)
  instance
  {-# OVERLAPPABLE #-}
    (AllBF Semigroup f JobInfoF) => Semigroup (JobInfoF f)

deriving via
  GenericSemigroupMonoid (JobInfoF Last)
  instance
  {-# OVERLAPPING #-}
    Semigroup (JobInfoF Maybe)

deriving via
  GenericSemigroupMonoid (JobInfoF f)
  instance
  {-# OVERLAPPABLE #-}
    (AllBF Monoid f JobInfoF) => Monoid (JobInfoF f)

deriving via
  GenericSemigroupMonoid (JobInfoF Last)
  instance
  {-# OVERLAPPING #-}
    Monoid (JobInfoF Maybe)

deriving anyclass instance GStorable JobInfoStruct

newtype JobInfo = MkJobInfo {getJobInfo :: Ptr JobInfoStruct}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Storable)

type JobTemplateStruct = JobTemplateF Unpacked

data JobTemplateF f = JobTemplate
  { remoteCommand :: !(f BS.ByteString)
  , args :: !(f (V.Vector BS.ByteString))
  , submitAsHold :: !(f Bool)
  , rerunnable :: !(f Bool)
  , jobEnvironment :: !(f (HM.HashMap BS.ByteString BS.ByteString))
  , workingDirectory :: !(f BS.ByteString)
  , jobCategory :: !(f BS.ByteString)
  , email :: !(f (V.Vector BS.ByteString))
  , emailOnStarted :: !(f Bool)
  , emailOnTerminated :: !(f Bool)
  , jobName :: !(f BS.ByteString)
  , inputPath :: !(f BS.ByteString)
  , outputPath :: !(f BS.ByteString)
  , errorPath :: !(f BS.ByteString)
  , joinFiles :: !(f Bool)
  , reservationId :: !(f BS.ByteString)
  , queueName :: !(f BS.ByteString)
  , minSlots :: !(f Int64)
  , maxSlots :: !(f Int64)
  , priority :: !(f Int64)
  , candidateMachines :: !(f (V.Vector BS.ByteString))
  , minPhysMemory :: !(f Int64)
  , machineOS :: !(f OS)
  , machineArch :: !(f CPU)
  , startTime :: !(f UTCTime)
  , deadlineTime :: !(f UTCTime)
  , stageInFiles :: !(f (HashMap BS.ByteString BS.ByteString))
  , stageOutFiles :: !(f (HashMap BS.ByteString BS.ByteString))
  , resourceLimits :: !(f (HashMap BS.ByteString BS.ByteString))
  , accountingId :: !(f BS.ByteString)
  }
  deriving (Generic)
  deriving anyclass (FunctorB, TraversableB, ConstraintsB, ApplicativeB)

deriving instance (AllBF Show f JobTemplateF) => Show (JobTemplateF f)

deriving instance (AllBF Eq f JobTemplateF) => Eq (JobTemplateF f)

deriving instance (AllBF Ord f JobTemplateF) => Ord (JobTemplateF f)

deriving anyclass instance (AllBF NFData f JobTemplateF) => NFData (JobTemplateF f)

deriving anyclass instance (AllBF Hashable f JobTemplateF) => Hashable (JobTemplateF f)

deriving via
  GenericSemigroupMonoid (JobTemplateF f)
  instance
  {-# OVERLAPPABLE #-}
    (AllBF Semigroup f JobTemplateF) => Semigroup (JobTemplateF f)

deriving via
  GenericSemigroupMonoid (JobTemplateF Last)
  instance
  {-# OVERLAPPING #-}
    Semigroup (JobTemplateF Maybe)

deriving via
  GenericSemigroupMonoid (JobTemplateF f)
  instance
  {-# OVERLAPPABLE #-}
    (AllBF Monoid f JobTemplateF) => Monoid (JobTemplateF f)

deriving via
  GenericSemigroupMonoid (JobTemplateF Last)
  instance
  {-# OVERLAPPING #-}
    Monoid (JobTemplateF Maybe)

deriving anyclass instance GStorable JobTemplateStruct

deriving anyclass instance
  (AllBF J.FromJSON f JobTemplateF) => J.FromJSON (JobTemplateF f)

deriving anyclass instance
  (AllBF J.ToJSON f JobTemplateF) => J.ToJSON (JobTemplateF f)

newtype JobTemplate = MkJobTemplate {getJobTemplate :: Ptr JobTemplateStruct}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Storable)

drmaa2StructTypeTable :: C.TypesTable
drmaa2StructTypeTable =
  Map.fromList
    [ (C.TypeName "drmaa2_jtemplate_s", [t|JobTemplateStruct|])
    , (C.TypeName "drmaa2_jtemplate", [t|JobTemplate|])
    , (C.TypeName "drmaa2_jinfo_s", [t|JobInfoStruct|])
    , (C.TypeName "drmaa2_jinfo", [t|JobInfo|])
    , (C.TypeName "drmaa2_j_list", [t|DRMAAList Job|])
    ]

drmaa2StructCtx :: C.Context
drmaa2StructCtx = mempty {C.ctxTypesTable = drmaa2StructTypeTable}
