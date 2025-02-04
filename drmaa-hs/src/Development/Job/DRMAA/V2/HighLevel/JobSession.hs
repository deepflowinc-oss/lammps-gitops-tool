{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Job.DRMAA.V2.HighLevel.JobSession (
  JobSession,
  JobArray,

  -- * Creation and destruction
  createJobSession,
  openJobSession,
  closeJobSession,
  destroyJobSession,
  withJobSession,
  SessionOption (..),
  SessionPolicy (..),
  defaultSessionOption,

  -- * Queries
  getSessionName,
  getJobCategories,
  getJobs,
  getJobArray,
  getJobArrayJobs,

  -- * Job scheduling
  runJob,

  -- * Job-related APIs
  Job,
  JobTemplateF (..),
  PartialJobTemplate,
) where

import Control.DeepSeq (NFData)
import Control.Exception (evaluate)
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
import qualified Development.Job.DRMAA.V2.LowLevel as Low
import Development.Job.DRMAA.V2.LowLevel.Error
import Development.Job.DRMAA.V2.LowLevel.ImplementationSpecific
import qualified Development.Job.DRMAA.V2.LowLevel.Job as Raw
import Development.Job.DRMAA.V2.LowLevel.JobSession ()
import qualified Development.Job.DRMAA.V2.LowLevel.JobSession as Low.JS hiding (JobArray)
import Development.Job.DRMAA.V2.LowLevel.List (useListAsMaybeVector)
import qualified Development.Job.DRMAA.V2.LowLevel.Types as Low.Types
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import Development.Job.DRMAA.V2.Managed
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import GHC.Stack (HasCallStack, callStack)

newtype JobSession = JobSession {unJobSession :: Low.JobSession}
  deriving (Eq, Ord, Show)
  deriving newtype (NFData)

newtype JobArray = JobArray {unJobArray :: Low.JobArray}
  deriving (Eq, Ord, Show)
  deriving newtype (NFData)

{-
instance Show JobSession where
  showsPrec _ = const $ showString "<JobSession>"
 -}

{- |
Creates a new and persistent job session on the Altair Grid Engine master and opens it.
For successful creation a session with the name may not already exist.
-}
createJobSession :: BS.ByteString -> IO JobSession
createJobSession name =
  fmap JobSession . maybeToDRMAExceptionThrow =<< Low.JS.create name

{- |
Opens a DRMAA2 job session with a specific name.
The job session must exist on the Altair Grid Engine master process.
A job session can be created by the 'createJobSession' function or by @qconf(3)@ commands on command line.
The session name argument must not be empty.
In case of success a connection is established to the Altair Grid Engine master process.
If there is already a connection (because the process has already other job sessions or a monitoring session open) the existing connection to the master process is shared, but additionally events relating to jobs in this job session are subscribed from the master process.
The session needs to be closed and freed when it is not going to be used anymore in order to reduce the network traffic or close the connection to the master process completely (if the sesssion is the last open session).
-}
openJobSession :: BS.ByteString -> IO JobSession
openJobSession =
  maybeToDRMAExceptionThrow . fmap JobSession <=< Low.JS.open

{- |
Closes a job session.
Closing means that jobs submitted within this session can not be used / controlled / queried anymore.
Most 'JobSession' function calls will fail.
No further reports of jobs belonging to the job session are sent from the Altair Grid Engine master process to the DRMAA2 application.
If no other job session or monitoring session is open the DRMAA2 application is disconnected from the Altair Grid Engine master process.
-}
closeJobSession :: JobSession -> IO ()
closeJobSession =
  throwOnError <=< Low.JS.close . unJobSession

destroyJobSession :: BS.ByteString -> IO ()
destroyJobSession =
  throwOnError <=< Low.JS.destroy

data SessionPolicy = AlwaysCreate | CreateIfAbsent | AlwaysOpen
  deriving (Show, Eq, Ord, Generic)

{- |
Options for 'withJobSession'.

See also: 'defaultSessionOption' and 'withJobSession'.
-}
data SessionOption = SessionOption
  { closeFinally :: !Bool
  -- ^ Closing job at last, making all submitted job uncontrollable even in succeeding sessions with the same name (default: 'False').
  , destroyFinally :: !Bool
  -- ^ Destoys job session at the end (default: 'False').
  --
  -- WARNING: If there is other connections to the job session, this can lead to unexpected behaviour.
  , sessionPolicy :: !SessionPolicy
  -- ^ JobSession creation strategy (default: 'CreateIfAbsent')
  }
  deriving (Show, Eq, Ord, Generic)

defaultSessionOption :: SessionOption
defaultSessionOption =
  SessionOption
    { destroyFinally = False
    , closeFinally = False
    , sessionPolicy = CreateIfAbsent
    }

{- |
Opens or Creates 'JobSession' with the specified name and pass it as a continuation.
Session will be automatically closed (and destroyed if 'destroyFinally' is 'True').

See Also: 'SessionOption' and 'defaultSessionOption'.
-}
withJobSession ::
  (HasCallStack, MonadMask m, MonadIO m) =>
  SessionOption ->
  BS.ByteString ->
  (JobSession -> m b) ->
  m b
withJobSession SessionOption {..} name k =
  either throwM pure =<< uninterruptibleMask \restore -> do
    mjs <- liftIO $ case sessionPolicy of
      AlwaysCreate -> do
        Low.JS.create name
      AlwaysOpen -> do
        Low.JS.open name
      CreateIfAbsent -> do
        Low.JS.open name >>= \case
          Nothing -> do
            Low.JS.create name
          Just js -> do
            pure $ Just js
    E.runExceptT $ case mjs of
      Nothing -> do
        E.throwE . fromMaybe (ErrorInfoLost callStack)
          =<< liftIO getLastException
      Just js -> do
        a <- lift $ restore $ k $ JobSession js
        when closeFinally $
          mapM_ E.throwE =<< liftIO (reifyOnError =<< Low.JS.close js)
        when destroyFinally $
          mapM_ E.throwE =<< liftIO (reifyOnError =<< Low.JS.destroy name)
        pure a

getSessionName :: JobSession -> IO BS.ByteString
getSessionName = Low.JS.getName . unJobSession

getJobCategories :: (HasCallStack) => JobSession -> IO (V.Vector BS.ByteString)
getJobCategories js = do
  jobCats <- Low.JS.getJobCategories $ unJobSession js
  maybeToDRMAExceptionThrow
    =<< useListAsMaybeVector Free jobCats pure

getJobs :: (HasCallStack) => JobSession -> Maybe JobFilter -> IO (V.Vector Job)
getJobs js mfilter =
  withNullableUnpackedWithImplSpecMoved
    (getField @"getJobInfo" <$> createJobInfo)
    mfilter
    $ \filt -> do
      jobs <- Low.JS.getJobs (unJobSession js) filt
      maybeToDRMAExceptionThrow
        =<< useListAsMaybeVector
          Free
          jobs
          (mapM $ V.mapM $ fmap Job . maybeToDRMAExceptionThrow <=< unsafeUnmanaged . getField @"getJob")

runJob :: (HasCallStack) => JobSession -> JobTemplate -> IO Job
runJob session template = withUnpackedWithImplSpecMoved
  (getField @"getJobTemplate" <$> Raw.createJobTemplate)
  template
  $ \tplt ->
    evaluate . Job
      =<< maybeToDRMAExceptionThrow
      =<< Low.JS.runJob (unJobSession session) tplt

getJobArray :: (HasCallStack) => JobSession -> BS.ByteString -> IO JobArray
getJobArray js n =
  fmap JobArray . maybeToDRMAExceptionThrow
    =<< Low.JS.getJobArray (unJobSession js) n

getJobArrayJobs :: (HasCallStack) => JobArray -> IO (V.Vector Job)
getJobArrayJobs ja = do
  jobs <- Low.JS.getJobArrayJobs (unJobArray ja)
  maybeToDRMAExceptionThrow
    =<< useListAsMaybeVector
      Free
      jobs
      (mapM $ V.mapM $ fmap Job . maybeToDRMAExceptionThrow <=< unsafeUnmanaged . getField @"getJob")
