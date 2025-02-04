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

module Development.Job.DRMAA.V2.LowLevel.MonitorSession (
  -- * Job-session types
  MonitorSession,
  JobInfo,

  -- ** Creation and destruction
  open,
  close,

  -- ** Information
  getAllJobs,

  -- * Data-types
) where

import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Development.Job.DRMAA.V2.LowLevel.Error
import Development.Job.DRMAA.V2.LowLevel.Job
import Development.Job.DRMAA.V2.LowLevel.List
import qualified Development.Job.DRMAA.V2.LowLevel.Types as Raw
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Development.Job.DRMAA.V2.LowLevel.Types.OpaqueStructs (opaqueStructCtx)
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
          , (C.TypeName "drmaa2_msession", [t|Raw.MonitorSession|])
          , (C.Struct "drmaa2_msession_s", [t|Raw.MonitorSessionStruct|])
          ]
    }
C.include "<drmaa2.h>"
C.include "<string.h>"

type MonitorSession = Managed Raw.MonitorSessionStruct

{- |
Closes a job session.
Closing means that jobs submitted within this session can not be used / controlled / queried anymore.
Most 'JobSession' function calls will fail.
No further reports of jobs belonging to the job session are sent from the Altair Grid Engine master process to the DRMAA2 application.
If no other job session or monitoring session is open the DRMAA2 application is disconnected from the Altair Grid Engine master process.
After closing the job session the job session object must be freed using the 'freeJobSession' call.
-}
close :: MonitorSession -> IO Error
close msession =
  [C.exp| drmaa2_error {
    drmaa2_close_msession($fptr-ptr:(struct drmaa2_msession_s *msession))
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
open :: BS.ByteString -> IO (Maybe MonitorSession)
open sessionName =
  toManaged
    =<< [C.exp| drmaa2_msession { drmaa2_open_msession(strdup($bs-cstr:sessionName)) } |]

getAllJobs :: MonitorSession -> JobInfo -> IO (DRMAAList Raw.Job)
getAllJobs session filter_ =
  [C.exp|drmaa2_j_list {
    drmaa2_msession_get_all_jobs(
      $fptr-ptr:(struct drmaa2_msession_s *session),
      $fptr-ptr:(drmaa2_jinfo_s *filter_)
    )
  }|]
