-- | A high-level interface for DRMAAv2 wrapping around LowLevel.
module Development.Job.DRMAA.V2.HighLevel (
  -- * Basic Types
  Error (..),
  DRMAAException (..),

  -- * Job Session Interface
  JobSession,
  WithImplSpec (..),

  -- ** Creation and destruction
  createJobSession,
  openJobSession,
  closeJobSession,
  destroyJobSession,
  withJobSession,
  SessionOption (..),
  SessionPolicy (..),
  defaultSessionOption,

  -- ** Queries
  getSessionName,
  getJobCategories,
  getJobs,

  -- ** Job scheduling
  runJob,

  -- * Job-related APIs
  Job,

  -- ** Job Information Query
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

  -- ** Job manipulation
  suspend,
  resume,
  hold,
  release,
  terminate,

  -- ** Waiting for a Job Status
  waitStarted,
  waitTerminated,

  -- ** Re-exports
  OS (..),
  CPU (..),
  JobState (..),
) where

import Development.Job.DRMAA.V2.HighLevel.Job
import Development.Job.DRMAA.V2.HighLevel.JobSession
import Development.Job.DRMAA.V2.LowLevel (DRMAAException (..), WithImplSpec (..))
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
