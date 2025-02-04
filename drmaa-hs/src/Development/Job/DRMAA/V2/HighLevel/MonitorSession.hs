{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Job.DRMAA.V2.HighLevel.MonitorSession (
  MonitorSession,

  -- * Creation and destruction
  openMonitorSession,
  closeMonitorSession,
  withMonitorSession,

  -- * Queries
  getAllJobs,

  -- * Job-related APIs
  Job,
  JobTemplateF (..),
  PartialJobTemplate,
) where

import Control.DeepSeq (NFData)
import Control.Exception.Safe
import Control.Monad hiding (mfilter)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Trans.Class (MonadTrans (..))
import qualified Control.Monad.Trans.Except as E
import qualified Data.ByteString as BS
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import Development.Job.DRMAA.V2.HighLevel.Job
import Development.Job.DRMAA.V2.LowLevel (createJobInfo)
import Development.Job.DRMAA.V2.LowLevel.Error
import Development.Job.DRMAA.V2.LowLevel.ImplementationSpecific
import Development.Job.DRMAA.V2.LowLevel.List (useListAsMaybeVector)
import qualified Development.Job.DRMAA.V2.LowLevel.MonitorSession as Low.MS
import qualified Development.Job.DRMAA.V2.LowLevel.Types as Low.Types
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import Development.Job.DRMAA.V2.Managed
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack, callStack)

newtype MonitorSession = MonitorSession {unMonitorSession :: Low.MS.MonitorSession}
  deriving (Eq, Ord, Show)
  deriving newtype (NFData)

{-
instance Show MonitorSession where
  showsPrec _ = const $ showString "<MonitorSession>"
 -}

{- |
Creates a new and persistent job session on the Altair Grid Engine master and opens it.
For successful creation a session with the name may not already exist.
-}
openMonitorSession :: BS.ByteString -> IO MonitorSession
openMonitorSession name =
  fmap MonitorSession . maybeToDRMAExceptionThrow =<< Low.MS.open name

{- |
Closes a job session.
Closing means that jobs submitted within this session can not be used / controlled / queried anymore.
Most 'MonitorSession' function calls will fail.
No further reports of jobs belonging to the job session are sent from the Altair Grid Engine master process to the DRMAA2 application.
If no other job session or monitoring session is open the DRMAA2 application is disconnected from the Altair Grid Engine master process.
-}
closeMonitorSession :: MonitorSession -> IO ()
closeMonitorSession =
  throwOnError <=< Low.MS.close . unMonitorSession

{- |
Opens or Creates 'MonitorSession' with the specified name and pass it as a continuation.
Session will be automatically closed (and destroyed if 'destroyFinally' is 'True').

See Also: 'SessionOption' and 'defaultSessionOption'.
-}
withMonitorSession ::
  (HasCallStack, MonadMask m, MonadIO m) =>
  BS.ByteString ->
  (MonitorSession -> m b) ->
  m b
withMonitorSession name k =
  either throwM pure =<< uninterruptibleMask \restore -> do
    mjs <- liftIO $ Low.MS.open name
    E.runExceptT $ case mjs of
      Nothing -> do
        E.throwE . fromMaybe (ErrorInfoLost callStack)
          =<< liftIO getLastException
      Just js -> do
        a <- lift $ restore $ k $ MonitorSession js
        mapM_ E.throwE =<< liftIO (reifyOnError =<< Low.MS.close js)
        pure a

getAllJobs :: MonitorSession -> Maybe JobFilter -> IO (V.Vector Job)
getAllJobs js mfilter =
  withNullableUnpackedWithImplSpecMoved
    (getField @"getJobInfo" <$> createJobInfo)
    mfilter
    $ \filt -> do
      jobs <- Low.MS.getAllJobs (unMonitorSession js) filt
      maybeToDRMAExceptionThrow
        =<< useListAsMaybeVector
          Free
          jobs
          (mapM $ V.mapM $ fmap Job . maybeToDRMAExceptionThrow <=< unsafeUnmanaged . getField @"getJob")
