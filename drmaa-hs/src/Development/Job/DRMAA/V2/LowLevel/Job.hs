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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fobject-code #-}

module Development.Job.DRMAA.V2.LowLevel.Job (
  Job (),
  getID,
  getSessionName,
  createJobTemplate,
  getJobTemplate,
  JobTemplate,
  suspend,
  resume,
  hold,
  release,
  terminate,
  waitStarted,
  waitTerminated,
  getState,
  JobState (..),
  getInfo,
  JobInfo,

  -- ** Constants
  zeroTime,
  infiniteTime,
  createJobInfo,
) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Development.Job.DRMAA.V2.LowLevel.String
import Development.Job.DRMAA.V2.LowLevel.Types (drmaa2BasicTypeCtx)
import qualified Development.Job.DRMAA.V2.LowLevel.Types as Raw
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Development.Job.DRMAA.V2.LowLevel.Types.Misc
import Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs (opaqueStructCtx)
import Development.Job.DRMAA.V2.Managed
import Foreign.C
import qualified Language.C.Inline as C hiding (exp, pure)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Interruptible as C
import qualified Language.C.Inline.Unsafe as UC
import qualified Language.C.Types as C

-- N.B.: HLS won't be happy when we use 'drmaa2Ctx' instead,
-- although GHC itself is happy with it...
C.context
  $ C.baseCtx
  <> C.funCtx
  <> C.fptrCtx
  <> opaqueStructCtx
  <> drmaa2BasicTypeCtx
  <> drmaa2EnumCtx
  <> mempty
    { C.ctxTypesTable =
        Map.fromList
          [ (C.TypeName "drmaa2_jtemplate_s", [t|Raw.JobTemplateStruct|])
          , (C.TypeName "drmaa2_jinfo_s", [t|Raw.JobInfoStruct|])
          , (C.Struct "drmaa2_j_s", [t|Raw.JobStruct|])
          , (C.TypeName "drmaa2_jinfo", [t|Raw.JobInfo|])
          , (C.TypeName "drmaa2_jtemplate", [t|Raw.JobTemplate|])
          ]
    }
C.include "<drmaa2.h>"

type Job = Managed Raw.JobStruct

type JobTemplate = Managed Raw.JobTemplateStruct

getID :: Job -> IO (Maybe BS.ByteString)
getID job =
  fromDRMAAStringMaybe Free
    =<< [C.exp| drmaa2_string {
          drmaa2_j_get_id($fptr-ptr:(struct drmaa2_j_s *job))
        }|]

getSessionName :: Job -> IO (Maybe BS.ByteString)
getSessionName job =
  fromDRMAAStringMaybe Free
    =<< [C.exp| drmaa2_string {
          drmaa2_j_get_session_name($fptr-ptr:(struct drmaa2_j_s *job))
        }|]

getJobTemplate :: Job -> IO (Maybe JobTemplate)
getJobTemplate job =
  toManaged
    =<< [C.exp|drmaa2_jtemplate {
          drmaa2_j_get_jtemplate($fptr-ptr:(struct drmaa2_j_s *job))
        }|]

suspend :: Job -> IO Error
suspend job =
  [C.exp|drmaa2_error {
    drmaa2_j_suspend($fptr-ptr:(struct drmaa2_j_s *job))
  }|]

resume :: Job -> IO Error
resume job =
  [C.exp|drmaa2_error {
    drmaa2_j_resume($fptr-ptr:(struct drmaa2_j_s *job))
  }|]

hold :: Job -> IO Error
hold job =
  [C.exp|drmaa2_error {
    drmaa2_j_hold($fptr-ptr:(struct drmaa2_j_s *job))
  }|]

release :: Job -> IO Error
release job =
  [C.exp|drmaa2_error {
    drmaa2_j_release($fptr-ptr:(struct drmaa2_j_s *job))
  }|]

terminate :: Job -> IO Error
terminate job =
  [C.exp|drmaa2_error {
    drmaa2_j_terminate($fptr-ptr:(struct drmaa2_j_s *job))
  }|]

zeroTime :: CTime
zeroTime = [UC.pure|time_t {DRMAA2_ZERO_TIME}|]

infiniteTime :: CTime
infiniteTime = [UC.pure|time_t {DRMAA2_INFINITE_TIME}|]

{- |
This function blocks until the given job is in started state.
The function should be called for jobs in 'Queued' and 'QueuedHeld' states.
It immediately returns when the job is in any finished or unknown state ('Failed', 'Done', 'Undetermined').
The timeout argument determines a maximum time - in seconds - the function should block.
Special constant values are 'zeroTime' (@DRMAA2_ZERO_TIME@) and 'infiniteTime' (@DRMAA2_INFINITE_TIME@).
If 'zeroTime' is used as argument, the function returns immediately after checking the job state.
If the job is not in the expected state the return value is @/= 'Success'@ (when no other error happend it is 'InvalidState').
When using 'infiniteTime' the function blocks possibly endlessly.

Returns 'Success' when the job is in any or passed a started state.
Otherwise an error code is returned.
The error reason is saved in the contex of the calling thread and can be fetched with the 'Development.Job.DRMAA.V2.LowLevel.Error.lastErrorText' function.
-}
waitStarted ::
  Job ->
  CTime ->
  -- | Returns 'Success' when the job is in any or passed a started state;
  -- returns error code otherwise.
  IO Error
waitStarted job timeout =
  -- NOTE: We must use Interruptible FFI here, as waitStarted
  -- can block indefinitely.
  [C.exp|drmaa2_error {
    drmaa2_j_wait_started($fptr-ptr:(struct drmaa2_j_s *job), $(const time_t timeout))
  }|]

{- |
This function blocks until the given job is in any terminated state.
It also immediately returns when the job is in unknown state ('Undetermined').
The timeout argument determines a maximum time - in seconds - the function should block.
Special constant values are 'zeroTime' and 'infiniteTime'.
If 'zeroTime' is used as argument, the function returns immediately after checking the job state.
If the job is not in the expected state the return value is @/= 'Success'@(when no other error happend it is 'InvalidState').
When using 'infiniteTime' the function blocks possibly endlessly.
-}
waitTerminated :: Job -> CTime -> IO Error
waitTerminated job timeout =
  -- NOTE: We must use Interruptible FFI here, as waitStarted
  -- can block indefinitely.
  [C.exp|drmaa2_error {
    drmaa2_j_wait_terminated($fptr-ptr:(struct drmaa2_j_s *job), $(const time_t timeout))
  }|]

{- |
Determines the current state of a DRMAA2 job.
Since the Altair Grid Engine DRMAA2 implemenation is event based,
the function call does not result in any addionaly communication to the Altair Grid Engine master process.
Hence this function can be safely called in a loop while waiting for a specific job state.
-}
getState :: Job -> IO JobState
getState job =
  [C.exp|drmaa2_jstate {
    drmaa2_j_get_state(
      $fptr-ptr:(struct drmaa2_j_s *job), NULL
    )
  }
  |]

type JobInfo = Managed Raw.JobInfoStruct

getInfo :: Job -> IO (Maybe JobInfo)
getInfo job =
  toManaged
    =<< [C.exp|drmaa2_jinfo {
        drmaa2_j_get_info($fptr-ptr:(struct drmaa2_j_s *job))
      }|]

createJobTemplate :: IO Raw.JobTemplate
createJobTemplate = [C.exp|drmaa2_jtemplate {drmaa2_jtemplate_create()}|]

createJobInfo :: IO Raw.JobInfo
createJobInfo = [C.exp|drmaa2_jinfo {drmaa2_jinfo_create()}|]
