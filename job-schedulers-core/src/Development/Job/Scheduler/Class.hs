{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.Job.Scheduler.Class (
  JobScheduler (..),
  JobSpec (..),
  JobInfo (..),
  JobStatus (..),
  Duration (..),
  parseDuration,
  withSession,
  JobSchedulerException (..),
) where

import Control.Applicative ((<|>))
import Control.DeepSeq
import Control.Exception.Safe
import qualified Control.Foldl as L
import Control.Monad
import Data.Aeson (FromJSON, ToJSON, parseJSON)
import qualified Data.Aeson as J
import Data.HashMap.Strict (HashMap)
import Data.Hashable
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.Time.Clock (NominalDiffTime)
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Text.Read (readMaybe)

data JobSpec = JobSpec
  { jobName :: !Text
  , command :: !FilePath
  , args :: ![Text]
  , workdir :: !FilePath
  , duration :: !(Maybe Duration)
  , environment :: !(HashMap Text Text)
  , numNodes :: !(Maybe Word)
  , coresPerNode :: !(Maybe Word)
  , memory :: !(Maybe Word)
  , gpus :: !(Maybe Word)
  , queue :: !(Maybe Text)
  , priority :: !(Maybe Int)
  , stdoutLog :: !(Maybe FilePath)
  , stderrLog :: !(Maybe FilePath)
  , joinLogs :: !(Maybe Bool)
  , startTime :: !(Maybe UTCTime)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, FromJSON, ToJSON)

data JobStatus
  = Queued
  | Suspended
  | Held
  | Cancelled !(Maybe String)
  | Running
  | Success
  | Aborted
  | Errored !String
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable, FromJSON, ToJSON)

data JobSchedulerException
  = JobNotFound !String
  | JobSchedulingFailed !String
  | SchedulerError !String
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

newtype Duration = Duration {durInSec :: Word}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Num, Hashable, NFData)

instance FromJSON Duration where
  parseJSON obj =
    Duration <$> J.parseJSON obj
      <|> J.withText "[[HH:]MM:]SS" (either fail pure . parseDuration . T.unpack) obj

instance ToJSON Duration where
  toJSON (Duration s) =
    let (mins0, secs) = s `quotRem` 60
        (hrs, mins) = mins0 `quotRem` 60
     in if s < 60
          then J.toJSON s
          else
            J.toJSON $
              shows hrs . showChar ':' . shows mins . showChar ':' . shows secs $
                ""

parseDuration :: String -> Either String Duration
parseDuration str =
  maybe (Left $ "Duration must be given in seconds or of form [[HH:]:MM]:SS, but got: " <> show str) Right $
    Duration <$> readMaybe str <|> readHHMMDD str

readHHMMDD :: String -> Maybe Duration
readHHMMDD str = do
  let comps = T.splitOn ":" $ T.pack str
  (len, secs) <-
    L.foldM
      ( (,)
          <$> L.generalize L.length
          <*> L.FoldM (\l r -> ((l * 60) +) <$> readMaybe (T.unpack r)) (Just 0) (pure . Duration)
      )
      comps
  guard $ 0 < len && len < 4
  pure secs

data JobInfo s = JobInfo
  { jobID :: !Text
  , jobName :: !Text
  , jobStatus :: !JobStatus
  , annotation :: !(Maybe Text)
  , exitCode :: !(Maybe Int)
  , queueName :: !(Maybe Text)
  , wallclockTime :: !(Maybe NominalDiffTime)
  , cpuTime :: !(Maybe Int)
  , submittedAt :: !UTCTime
  , startedAt :: !(Maybe UTCTime)
  , finishedAt :: !(Maybe UTCTime)
  , customInfo :: !s
  }
  deriving (Show, Eq, Ord, Generic, Functor, Foldable, Traversable)
  deriving anyclass (NFData, Hashable, FromJSON, ToJSON)

class (Monad m) => JobScheduler s m where
  data Session s
  data SchedulerConfig s
  data Job s
  data CustomJobInfo s
  newSession :: SchedulerConfig s -> m (Session s)
  closeSession :: Session s -> m ()
  scheduleJob :: Session s -> JobSpec -> m (Job s)
  getJobInfo :: Session s -> Job s -> m (JobInfo (CustomJobInfo s))
  getJobStatus :: Session s -> Job s -> m JobStatus
  getJobStatus = fmap (fmap (getField @"jobStatus")) . getJobInfo

  -- | Blocks until start or timeout.
  -- Returns 'Nothing' on timeout.
  waitStarted :: Session s -> Maybe NominalDiffTime -> Job s -> m (Maybe JobStatus)

  -- | Blocks until terminate or timeout.
  -- Returns 'Nothing' on timeout.
  waitTerminated :: Session s -> Maybe NominalDiffTime -> Job s -> m (Maybe JobStatus)

  cancel :: Session s -> Job s -> m ()

withSession :: (JobScheduler s m, MonadMask m) => SchedulerConfig s -> (Session s -> m a) -> m a
withSession cfg = bracket (newSession cfg) closeSession
