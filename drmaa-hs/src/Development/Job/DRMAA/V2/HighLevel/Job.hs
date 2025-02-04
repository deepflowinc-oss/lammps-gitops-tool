{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}

-- | A high-level interface for DRMAAv2.
module Development.Job.DRMAA.V2.HighLevel.Job (
  Job (..),

  -- * Job Information Query
  getJobID,
  getJobSessionName,
  JobInfoF (..),
  JobInfo,
  JobFilter,
  getJobInfo,
  JobTemplateF (..),
  JobTemplate,
  PartialJobTemplate,
  getJobTemplate,
  getJobState,

  -- * Job manipulation
  suspend,
  resume,
  hold,
  release,
  terminate,

  -- * Waiting for a Job Status
  waitStarted,
  waitTerminated,

  -- * Re-exports
  OS (..),
  CPU (..),
  JobState (..),

  -- * Conversion
  parseJobInfo,
  parseJobTemplate,
) where

import Control.Arrow ((>>>))
import Control.DeepSeq (NFData)
import Control.Exception (throwIO)
import Control.Monad
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import Data.Time (NominalDiffTime)
import qualified Development.Job.DRMAA.V2.LowLevel as Low
import Development.Job.DRMAA.V2.LowLevel.Error
import Development.Job.DRMAA.V2.LowLevel.ImplementationSpecific
import qualified Development.Job.DRMAA.V2.LowLevel.Job as Low.Job
import Development.Job.DRMAA.V2.LowLevel.Types (JobInfoF (..), JobTemplateF (..))
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Development.Job.DRMAA.V2.Managed
import Foreign
import Foreign.C
import GHC.Stack (HasCallStack, callStack)

newtype Job = Job {unJob :: Low.Job}
  deriving (Eq, Ord)
  deriving newtype (NFData)

instance Show Job where
  showsPrec _ (Job a) = showString "<JOB: " . shows a . showString ">"
  {-# INLINE showsPrec #-}

getJobID :: Job -> IO BS.ByteString
getJobID = maybeToDRMAExceptionThrow <=< Low.Job.getID . unJob

getJobSessionName :: Job -> IO BS.ByteString
getJobSessionName = maybeToDRMAExceptionThrow <=< Low.Job.getSessionName . unJob

getJobInfo :: Job -> IO (Maybe JobInfo)
getJobInfo = mapM parseJobInfo <=< Low.Job.getInfo . unJob

getJobTemplate :: Job -> IO PartialJobTemplate
getJobTemplate = parseJobTemplate <=< maybeToDRMAExceptionThrow <=< Low.Job.getJobTemplate . unJob

getJobState :: (HasCallStack) => Job -> IO JobState
getJobState =
  unJob >>> Low.getState >=> \case
    Undetermined -> throwIO . CouldNotDetermineJobState callStack . fromMaybe "<Unknown Reason>" =<< Low.lastErrorText
    s -> pure s

suspend :: Job -> IO ()
suspend = throwOnError <=< Low.suspend . unJob

resume :: Job -> IO ()
resume = throwOnError <=< Low.resume . unJob

hold :: Job -> IO ()
hold = throwOnError <=< Low.resume . unJob

release :: Job -> IO ()
release = throwOnError <=< Low.resume . unJob

terminate :: Job -> IO ()
terminate = throwOnError <=< Low.terminate . unJob

waitStarted :: Job -> Maybe NominalDiffTime -> IO ()
waitStarted job =
  throwOnError <=< Low.waitStarted (unJob job) . fromNomDiffTime

waitTerminated :: Job -> Maybe NominalDiffTime -> IO ()
waitTerminated job =
  throwOnError <=< Low.waitTerminated (unJob job) . fromNomDiffTime

fromNomDiffTime :: Maybe NominalDiffTime -> CTime
fromNomDiffTime =
  maybe
    Low.infiniteTime
    ( \w ->
        if w <= 0
          then Low.zeroTime
          else fromInteger $ floor w
    )

type PartialJobTemplate = WithImplSpec JobTemplateF Maybe

type JobTemplate = WithImplSpec JobTemplateF Maybe

parseJobTemplate :: Low.JobTemplate -> IO PartialJobTemplate
parseJobTemplate (Managed jtmplt) =
  withForeignPtr jtmplt bpackStructWithImplSpec

type JobInfo = WithImplSpec JobInfoF Maybe

type JobFilter = WithImplSpec JobInfoF Maybe

parseJobInfo :: Low.JobInfo -> IO JobInfo
parseJobInfo (Managed jiraw) = withForeignPtr jiraw bpackStructWithImplSpec
