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

module Development.Job.DRMAA.V2.LowLevel.JobSession (
  -- * Job-session types
  JobSession,
  Job,
  JobTemplate,
  JobArray,

  -- ** Creation and destruction
  create,
  open,
  close,
  destroy,

  -- ** Information
  getName,
  getContact,
  getJobCategories,
  getJobs,
  getJobArray,
  getJobArrayJobs,

  -- * Job scheduling
  runJob,

  -- * Data-types
) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Development.Job.DRMAA.V2.LowLevel.Error
import Development.Job.DRMAA.V2.LowLevel.Job
import Development.Job.DRMAA.V2.LowLevel.List
import Development.Job.DRMAA.V2.LowLevel.String
import qualified Development.Job.DRMAA.V2.LowLevel.Types as Raw
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs (opaqueStructCtx)
import qualified Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs as Raw
import Development.Job.DRMAA.V2.Managed
import Foreign.C (CInt (CInt))
import qualified Language.C.Inline as C hiding (exp, pure)
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Interruptible as C
import qualified Language.C.Types as C

-- N.B.: HLS won't be happy when we use 'drmaa2Ctx' instead,
-- although GHC itself is happy with it...
C.context
  $ C.baseCtx
  <> C.funCtx
  <> C.fptrCtx
  <> C.bsCtx
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
          , (C.TypeName "drmaa2_j_list", [t|DRMAAList Raw.Job|])
          , (C.TypeName "drmaa2_jarray", [t|Raw.JobArray|])
          , (C.TypeName "drmaa2_string", [t|DRMAAString|])
          ]
    }
C.include "<drmaa2.h>"
C.include "<string.h>"

type JobSession = Managed Raw.JobSessionStruct

type JobArray = Managed Raw.JobArrayStruct

{- |
@'create' sessionName contact@ creates a new and persistent job session on the Altair Grid Engine master and opens it.
For successful creation a session with the name may not already exist.
-}
create :: BS.ByteString -> IO (Maybe JobSession)
create sessionName = do
  toManaged
    =<< [C.exp| drmaa2_jsession { 
          drmaa2_create_jsession(strdup($bs-cstr:sessionName), NULL) }
        |]

{- |
Closes a job session.
Closing means that jobs submitted within this session can not be used / controlled / queried anymore.
Most 'JobSession' function calls will fail.
No further reports of jobs belonging to the job session are sent from the Altair Grid Engine master process to the DRMAA2 application.
If no other job session or monitoring session is open the DRMAA2 application is disconnected from the Altair Grid Engine master process.
After closing the job session the job session object must be freed using the 'freeJobSession' call.
-}
close :: JobSession -> IO Error
close jsession =
  [C.exp| drmaa2_error {
    drmaa2_close_jsession($fptr-ptr:(struct drmaa2_jsession_s *jsession))
  }|]

{- |
Opens a DRMAA2 job session with a specific name.
The job session must exist on the Altair Grid Engine master process.
A job session can be created by the 'createJobSession' function or by @qconf(3)@ commands on command line.
The session name argument must not be empty.
In case of success a connection is established to the Altair Grid Engine master process.
If there is already a connection (because the process has already other job sessions or a monitoring session open) the existing connection to the master process is shared, but additionally events relating to jobs in this job session are subscribed from the master process.
The session needs to be closed and freed when it is not going to be used anymore in order to reduce the network traffic or close the connection to the master process completely (if the sesssion is the last open session).
-}
open :: BS.ByteString -> IO (Maybe JobSession)
open sessionName =
  toManaged
    =<< [C.exp| drmaa2_jsession { drmaa2_open_jsession(strdup($bs-cstr:sessionName)) } |]

{- |
Removes a persistent DRMAA2 job session with the given session_name from the Altair Grid Engine qmaster process.
The session must belong to the user of the application otherwise a failure is returned.
Only Altair Grid Engine admins and operators are allowed to delete other user job sessions.
If a job session is removed while an other application uses it, the behaviour is undefined.
Existing DRMAA2 job session names can be fetched wth the 'getJobSessionNames' function.

Note: In Altair Grid Engine DRMAA2 job sessions can be listed, created, and deleted also by qconf(3) calls.
-}
destroy :: BS.ByteString -> IO Error
destroy sessionName =
  [C.exp| drmaa2_error { drmaa2_destroy_jsession(strdup($bs-cstr:sessionName)) } |]

{- |
Returns the contact string of a DRMAA2 job session.
In Altair Grid Engine a contact string is not used and therfore always empty string is returned.
-}
getContact :: JobSession -> IO (Maybe BS.ByteString)
getContact session = do
  fromDRMAAStringMaybe Free
    =<< [C.exp| drmaa2_string { 
          drmaa2_jsession_get_contact($fptr-ptr:(struct drmaa2_jsession_s *session))
        }|]

{- |
Returns the name of a DRMAA2 job session.
This name can be used for opening a job session after it was closed and it can be used for destroying a job session (after the session was closed).
The name is identical to the name which was used for creating the job session.
-}
getName :: JobSession -> IO BS.ByteString
getName session = do
  maybeToDRMAExceptionThrow
    =<< fromDRMAAStringMaybe Free
    =<< [C.exp| drmaa2_string { 
          drmaa2_jsession_get_session_name($fptr-ptr:(struct drmaa2_jsession_s *session))
        }|]

{- |
Returns a list of job categories available in the job session.
For Altair Grid Engine a job category is represented by a job class.
Hence requesting a job category in the drmaa2_jtemplate is equivalent to requesting a job class with qsub(3).
The job category list is a list of job classes defined in Altair Grid Engine.
If there is no job category defined in Altair Grid Engine the job category list is defined but empty (size of 0).
After a sucessful creation of the list (with 0 or more elements) the list must be freed by the caller after usage with the drmaa2_list_free(3) function.
The drmaa2_string_list is created with a callback function which frees all allocated strings.
-}
getJobCategories :: JobSession -> IO (DRMAAList DRMAAString)
getJobCategories session =
  [C.exp| drmaa2_string_list {
    drmaa2_jsession_get_job_categories($fptr-ptr:(struct drmaa2_jsession_s *session))
  }|]

{- |
Returns a list of jobs in a drmaa2_j_list which belong to the given job session. The job list contains only jobs which are submitted in the same job session.
If the application was restarted between job submission with drmaa2_jsession_run_job(3) and the drmaa2_jession_get_jobs(3) call the list contains only jobs which are still in Altair Grid Engine (meaning jobs finshed in between are not part of the job list).
When a job finishes while the job session is open, the finished job is stored together with its usage in the drmaa2_jsession until the session is closed by drmaa2_close_jsession(3).
Depending on the job state the drmaa2_j objects have varying information available. Freshly submitted jobs might have just a few of the fields set, while finsihed jobs have full usage information available.
Array jobs are returned as single, separated jobs.

The second argument is a filter for the job list.
If the filter is set to NULL all availble jobs are returned.
If the filter has some UNSET entries only jobs matching the filter are returned.
The filter works in the same way than for drmaa2_msession_get_all_jobs(3).
For more details about job filtering please consider the drmaa2_msession_get_all_jobs(3) man page.

Returns a newly allocated list of jobs which belong to the DRMAA2 job session.
The list has 0 or more entries.
In case of an failure NULL is returned and the error id and error string is set for the calling thread.
An appropriate callback function was set so that drmaa2_list_free(3) removes all allocated memory.
-}
getJobs :: JobSession -> JobInfo -> IO (DRMAAList Raw.Job)
getJobs session filter_ =
  [C.exp|drmaa2_j_list {
    drmaa2_jsession_get_jobs(
      $fptr-ptr:(struct drmaa2_jsession_s *session),
      $fptr-ptr:(drmaa2_jinfo_s *filter_)
    )
  }|]

getJobArray :: JobSession -> BS.ByteString -> IO (Maybe JobArray)
getJobArray session name = do
  toManaged
    =<< [C.exp|drmaa2_jarray {
          drmaa2_jsession_get_job_array(
            $fptr-ptr:(struct drmaa2_jsession_s *session),
            strdup($bs-cstr:name)
          )
        }|]

getJobArrayJobs :: JobArray -> IO (DRMAAList Raw.Job)
getJobArrayJobs session = do
  [C.exp|drmaa2_j_list {
    drmaa2_jarray_get_jobs(
      $fptr-ptr:(struct drmaa2_jarray_s *session)
    )
  }|]

{- |
This function submits a job in the given job session based on the job description provided by the job template.
For a successful job submission the given job session needs to be open. The returned job object can be used for monitoring and controlling the job.

Resource limits and general resource requests which are not part of the job template or the Altair Grid Engine job template enhancements (but which available in Altair Grid Engine installation with the @-l@ requests - like user defined complexes), can be requested by using the resourceLimits dictionary which is part of the DRMAA2 job template.

Returns a newly allocated DRMAA2 job object or NULL in case any error happend.
In case of an error the error ID and error text is stored in the context of the thread which submitted the job.
-}
runJob :: JobSession -> JobTemplate -> IO (Maybe Job)
runJob session tmplt =
  toManaged
    =<< [C.exp| drmaa2_j {
        drmaa2_jsession_run_job(
          $fptr-ptr:(struct drmaa2_jsession_s *session),
          $fptr-ptr:(drmaa2_jtemplate_s *tmplt)
        )
      }|]
