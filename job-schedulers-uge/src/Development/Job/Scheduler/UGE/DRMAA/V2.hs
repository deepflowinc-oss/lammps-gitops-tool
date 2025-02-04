{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Development.Job.Scheduler.UGE.DRMAA.V2 (
  UgeDrmaaV2,
  defaultConfig,
  JobScheduler (..),
  SchedulerConfig (..),
  CustomJobInfo (..),
) where

import Control.Concurrent.STM.TBMQueue
import Control.DeepSeq (NFData)
import Control.Monad (when)
import Control.Monad.Loops (whileJust_)
import Data.Aeson (FromJSON, ToJSON)
import Data.Align (Semialign (..))
import qualified Data.Bifunctor as Bi
import qualified Data.ByteString as BS
import qualified Data.ByteString.Encoding as BSE
import Data.Function (fix)
import Data.Functor ((<&>))
import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.These (These (..))
import Data.Time (NominalDiffTime, UTCTime (..))
import qualified Data.Vector as V
import Development.Job.DRMAA.V2 (JobTemplateF (..))
import qualified Development.Job.DRMAA.V2.HighLevel as DRMAA2
import Development.Job.DRMAA.V2.LowLevel (DRMAAException)
import Development.Job.Scheduler.Class
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import UnliftIO
import qualified UnliftIO.Async as Async
import UnliftIO.Concurrent (threadDelay)

data UgeDrmaaV2

defaultConfig :: Text -> SchedulerConfig UgeDrmaaV2
defaultConfig sessionName =
  UgeDrmaaV2Config
    { closeFinally = False
    , coreParallelEnv = Just "OpenMP"
    , nodeParallelEnv = Just "openmpi"
    , cancelCheckInterval = 60
    , sessionName
    }

encodeSystem :: Text -> BS.ByteString
encodeSystem = BSE.encode BSE.localeEncoding

decodeSystem :: BS.ByteString -> Text
decodeSystem = BSE.decode BSE.localeEncoding

data JobQuery
  = GetJobInfo
      !(Job UgeDrmaaV2)
      !(TMVar (JobInfo (CustomJobInfo UgeDrmaaV2)))
  | GetJobStatus
      !(Job UgeDrmaaV2)
      !(TMVar JobStatus)

instance (MonadUnliftIO m) => JobScheduler UgeDrmaaV2 m where
  newtype Job UgeDrmaaV2 = UgeDrmaaV2Job {getRawJob :: DRMAA2.Job}
  data Session UgeDrmaaV2 = UgeDrmaaV2Session
    { jobSession :: !DRMAA2.JobSession
    , config :: !(SchedulerConfig UgeDrmaaV2)
    , queryQueue :: !(TBMQueue JobQuery)
    , queryThread :: !(Async ())
    }

  data SchedulerConfig UgeDrmaaV2 = UgeDrmaaV2Config
    { sessionName :: !Text
    , cancelCheckInterval :: !NominalDiffTime
    , closeFinally :: !Bool
    , coreParallelEnv :: !(Maybe Text)
    , nodeParallelEnv :: !(Maybe Text)
    }
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, Hashable, NFData)

  data CustomJobInfo UgeDrmaaV2 = UgeDrmaaV2JobInfo
    deriving (Show, Eq, Ord, Generic)
    deriving anyclass (FromJSON, ToJSON, Hashable, NFData)

  newSession config@UgeDrmaaV2Config {..} = liftIO $ do
    -- NOTE: see NOTE on getJobInfo and getJobStatus
    queryQueue <- newTBMQueueIO 10
    queryThread <-
      asyncBound
        $ whileJust_ (atomically $ readTBMQueue queryQueue)
        $ \case
          GetJobInfo j sink ->
            atomically . putTMVar sink =<< getJobInfoImpl j
          GetJobStatus j sink ->
            atomically . putTMVar sink =<< getJobStatusM (getRawJob j)
    jobSession <-
      DRMAA2.openJobSession (encodeSystem sessionName)
        `catchAny` const (DRMAA2.createJobSession $ encodeSystem sessionName)
    pure UgeDrmaaV2Session {..}

  closeSession UgeDrmaaV2Session {config = UgeDrmaaV2Config {..}, ..} = do
    uninterruptibleMask_ $ do
      atomically $ closeTBMQueue queryQueue
      Async.cancel queryThread
    when closeFinally $ liftIO $ DRMAA2.closeJobSession jobSession

  -- NOTE: it seems that querying job info/status from multiple threads results in SEGV.
  -- So we create dedicated thread and request the result.

  getJobStatus UgeDrmaaV2Session {..} j = do
    sink <- newEmptyTMVarIO
    atomically $ writeTBMQueue queryQueue $ GetJobStatus j sink
    atomically $ takeTMVar sink

  -- NOTE: it seems that querying job info/status from multiple threads results in SEGV.
  -- So we create dedicated thread and request the result.
  getJobInfo UgeDrmaaV2Session {..} job = do
    sink <- newEmptyTMVarIO
    atomically $ writeTBMQueue queryQueue $ GetJobInfo job sink
    atomically $ takeTMVar sink

  scheduleJob UgeDrmaaV2Session {..} js = do
    eith <-
      try @_ @DRMAA2.DRMAAException
        $ liftIO
        . DRMAA2.runJob jobSession
        =<< either (throwIO . JobSchedulingFailed) pure (fromJobSpec config js)
    case eith of
      Left err -> throwIO $ JobSchedulingFailed $ displayException err
      Right j -> pure $ UgeDrmaaV2Job j

  -- NOTE: See the comment to 'waitForCancel' for the reason for 'withCancelMonitorTimeout'.
  waitStarted = withCancelMonitorTimeout DRMAA2.waitStarted

  -- NOTE: See the comment to 'waitForCancel' for the reason for 'withCancelMonitorTimeout'.
  waitTerminated = withCancelMonitorTimeout DRMAA2.waitTerminated

  cancel = const $ liftIO . DRMAA2.terminate . getRawJob

getJobInfoImpl :: Job UgeDrmaaV2 -> IO (JobInfo (CustomJobInfo UgeDrmaaV2))
getJobInfoImpl (UgeDrmaaV2Job job) = do
  DRMAA2.WithImplSpec j _ <-
    maybe (throwIO $ JobNotFound "Requested job not found") pure
      =<< liftIO (DRMAA2.getJobInfo job)
  let jobID = maybe "N/A" decodeSystem $ getField @"jobId" j
      jobName = maybe "N/A" decodeSystem $ getField @"jobName" j
      annotation = decodeSystem <$> getField @"annotation" j
      jobStatus = fromJobState $ fromJust $ getField @"jobState" j
      exitCode = fromIntegral <$> getField @"exitStatus" j
      queueName = decodeSystem <$> getField @"queueName" j
      wallclockTime = getField @"wallclockTime" j
      cpuTime = fromIntegral <$> getField @"cpuTime" j
      submittedAt = fromMaybe (UTCTime (toEnum 0) 0) $ getField @"submissionTime" j
      startedAt = getField @"dispatchTime" j
      finishedAt = getField @"finishTime" j
      customInfo = UgeDrmaaV2JobInfo
  pure JobInfo {..}

{- |
Internal function that wraps 'waitStarted' and 'waitTerminated' to handle cancellation before 'Running' properly.

In some version of UGE, cancellation before starting doesn't affect
'jobState' and/or 'jobSubState', i.e. remains to be @Queued@!
To workaround this, we use dedicated thread to periodically monitors
@uge_ji_failed@ implementation specific variable in jobinfo,
which is set to "-" for genuinely queued job and some other value for cancelled job.
-}
withCancelMonitorTimeout ::
  (MonadUnliftIO m) =>
  (DRMAA2.Job -> Maybe NominalDiffTime -> IO ()) ->
  Session UgeDrmaaV2 ->
  Maybe NominalDiffTime ->
  Job UgeDrmaaV2 ->
  m (Maybe JobStatus)
withCancelMonitorTimeout
  waiter
  UgeDrmaaV2Session {config = UgeDrmaaV2Config {..}}
  mtimeout
  (UgeDrmaaV2Job j) = do
    st <- liftIO $ DRMAA2.getJobState j
    if st >= DRMAA2.Running
      then withWaiterOutput =<< waiterThread
      else do
        eith <- waiterThread `race` waitForCancel
        case eith of
          Left i -> withWaiterOutput i
          Right ma -> pure $ Just ma
    where
      withWaiterOutput eith = case eith of
        Left err -> pure $ Just err
        Right () -> do
          st <- liftIO $ DRMAA2.getJobState j
          if st >= DRMAA2.Running
            then pure $ Just $ fromJobState st
            else pure Nothing
      waiterThread = do
        eith <- try @_ @DRMAAException $ liftIO $ waiter j mtimeout
        case eith of
          Left err -> pure $ Left (Errored $ displayException err)
          Right {} -> pure $ Right ()

      waitForCancel = fix $ \self -> do
        sleep cancelCheckInterval
        jinfo <- liftIO $ DRMAA2.getJobInfo j
        st <- liftIO $ DRMAA2.getJobState j
        if
          | st >= DRMAA2.Done -> pure $ fromJobState st
          | st < DRMAA2.Running
          , Just failCode <- HM.lookup "uge_ji_failed" =<< DRMAA2.implSpec =<< jinfo
          , failCode /= "-" ->
              pure
                $ Cancelled
                $ Just
                $ "UGE JOB seems cancelled. failCode = "
                <> show failCode
                <> "; state = "
                <> show st
          | otherwise -> self

getJobStatusM :: (MonadIO m) => DRMAA2.Job -> m JobStatus
getJobStatusM = liftIO . fmap fromJobState . DRMAA2.getJobState

sleep :: (MonadIO m) => NominalDiffTime -> m ()
sleep = threadDelay . toMicroSeconds

toMicroSeconds :: NominalDiffTime -> Int
toMicroSeconds = floor . (* 10 ^ (6 :: Int))

fromJobSpec ::
  SchedulerConfig UgeDrmaaV2 ->
  JobSpec ->
  Either String DRMAA2.JobTemplate
fromJobSpec UgeDrmaaV2Config {..} JobSpec {..} = do
  peSpec <-
    if
      | isNodeParallel ->
          maybe
            (Left "Multi-node is specified, but no `nodeParallelEnv' is specified")
            (pure . Just)
            nodeParallelEnv
      | isCoreParallel ->
          maybe
            (Left "Multi-core, single-node mode is specified, but no `coreParallelEnv' is specified")
            (pure . Just)
            coreParallelEnv
      | otherwise -> pure Nothing
  let implSpec = fmap (HM.singleton "uge_jt_pe" . encodeSystem) peSpec
      slots =
        alignWith
          ( \case
              This i -> fromIntegral i
              That i -> fromIntegral i
              These i j -> fromIntegral $ i * j
          )
          numNodes
          coresPerNode
      resLims =
        mconcat
          [ duration <&> \(Duration sec) ->
              HM.fromList [("h_rt", encodeSystem $ T.pack $ show sec)]
          , gpus <&> \i ->
              HM.fromList [("gpu", encodeSystem $ T.pack $ show i)]
          ]
  pure
    (mempty @DRMAA2.JobTemplate)
      { DRMAA2.implSpec = implSpec
      , DRMAA2.payload =
          JobTemplate
            { workingDirectory = Just $ encodeFP workdir
            , submitAsHold = Nothing
            , startTime = startTime
            , stageOutFiles = Nothing
            , stageInFiles = Nothing
            , resourceLimits = resLims
            , reservationId = Nothing
            , rerunnable = Nothing
            , remoteCommand = Just $ encodeFP command
            , args =
                if null args
                  then Nothing
                  else
                    Just
                      $ V.fromList
                      $ map encodeSystem args
            , queueName = encodeSystem <$> queue
            , priority = fromIntegral <$> priority
            , inputPath = Nothing
            , outputPath = encodeFP <$> stdoutLog
            , errorPath = encodeFP <$> stderrLog
            , joinFiles = joinLogs
            , minPhysMemory = fromIntegral <$> memory
            , minSlots = slots
            , maxSlots = slots
            , machineOS = Nothing
            , machineArch = Nothing
            , jobEnvironment =
                Just
                  $ HM.fromList
                  $ map (Bi.bimap encodeSystem encodeSystem)
                  $ HM.toList environment
            , jobCategory = Nothing
            , emailOnTerminated = Nothing
            , emailOnStarted = Nothing
            , email = Nothing
            , deadlineTime = Nothing
            , candidateMachines = Nothing
            , accountingId = Nothing
            , jobName = Just $ encodeSystem jobName
            }
      }
  where
    isNodeParallel = maybe False (> 1) numNodes
    isCoreParallel = maybe False (> 1) coresPerNode

encodeFP :: FilePath -> BS.ByteString
encodeFP = encodeSystem . T.pack

fromJobState :: DRMAA2.JobState -> JobStatus
fromJobState DRMAA2.Undetermined = Errored "Undetermined"
fromJobState DRMAA2.Queued = Queued
fromJobState DRMAA2.QueuedHeld = Held
fromJobState DRMAA2.Running = Running
fromJobState DRMAA2.Requeued = Queued
fromJobState DRMAA2.RequeuedHeld = Held
fromJobState DRMAA2.Suspended = Suspended
fromJobState DRMAA2.Done = Success
fromJobState DRMAA2.Failed = Aborted
