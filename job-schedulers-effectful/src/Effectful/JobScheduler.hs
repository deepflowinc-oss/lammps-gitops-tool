{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Effectful.JobScheduler (
  Scheduler,
  runSchedulerIO,
  runSchedulerPure,

  -- * Commands
  scheduleJob,
  getJobInfo,
  getJobStatus,
  waitStarted,
  waitTerminated,
  cancel,
  Job (),
  JobInfo (..),
  SimpleJobInfo,
  JobSpec (..),
  S.SchedulerConfig (),
  JobStatus (..),
) where

import Control.Exception.Safe (bracket)
import Data.Functor.Identity
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromJust)
import Data.Time (NominalDiffTime)
import Data.Void (Void)
import Development.Job.Scheduler.Class (JobInfo (..), JobSpec (..), JobStatus (..))
import qualified Development.Job.Scheduler.Class as S
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.STM (TVar, atomically, modifyTVar', newTVarIO, readTVar)
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local (evalState, gets, state)
import qualified StmContainers.Map as STMap

type instance DispatchOf Scheduler = 'Dynamic

data Scheduler :: Effect where
  ScheduleJob :: JobSpec -> Scheduler m Job
  GetJobInfo :: Job -> Scheduler m SimpleJobInfo
  GetJobStatus :: Job -> Scheduler m JobStatus
  WaitStarted :: Maybe NominalDiffTime -> Job -> Scheduler m (Maybe JobStatus)
  WaitTerminated :: Maybe NominalDiffTime -> Job -> Scheduler m (Maybe JobStatus)
  Cancel :: Job -> Scheduler m ()

type SimpleJobInfo = JobInfo (Maybe Void)

newtype Job = Job Int

instance Show Job where
  showsPrec _ Job {} = showString "<job>"

getJobInfo :: (HasCallStack, Scheduler :> es) => Job -> Eff es SimpleJobInfo
getJobInfo = send . GetJobInfo

getJobStatus :: (HasCallStack, Scheduler :> es) => Job -> Eff es JobStatus
getJobStatus = send . GetJobStatus

waitStarted :: (HasCallStack, Scheduler :> es) => Maybe NominalDiffTime -> Job -> Eff es (Maybe JobStatus)
waitStarted = fmap send . WaitStarted

waitTerminated :: (HasCallStack, Scheduler :> es) => Maybe NominalDiffTime -> Job -> Eff es (Maybe JobStatus)
waitTerminated = fmap send . WaitTerminated

cancel :: (HasCallStack, Scheduler :> es) => Job -> Eff es ()
cancel = send . Cancel

scheduleJob :: (HasCallStack, Scheduler :> es) => JobSpec -> Eff es Job
scheduleJob = send . ScheduleJob

runSchedulerIO ::
  forall s es a.
  (HasCallStack, S.JobScheduler s IO, IOE :> es, Concurrent :> es) =>
  S.SchedulerConfig s ->
  Eff (Scheduler ': es) a ->
  Eff es a
runSchedulerIO cfg act = do
  incr <- newTVarIO 0
  dic <- liftIO STMap.newIO
  bracket (liftIO $ S.newSession cfg) (liftIO . S.closeSession) $
    \sess -> loop incr dic sess act
  where
    loop :: TVar Int -> STMap.Map Int (S.Job s) -> S.Session s -> Eff (Scheduler ': es) x -> Eff es x
    loop idc dic sess =
      let getJob :: Job -> Eff es (S.Job s)
          getJob (Job i) = fmap fromJust $ atomically $ STMap.lookup i dic
       in interpret $ \_ -> \case
            ScheduleJob spec -> do
              rawJob <- liftIO $ S.scheduleJob sess spec
              atomically $ do
                i <- readTVar idc
                STMap.insert rawJob i dic
                modifyTVar' idc succ
                pure $ Job i
            GetJobInfo j -> do
              job <- getJob j
              (Nothing <$) <$> liftIO (S.getJobInfo sess job)
            GetJobStatus j -> do
              job <- getJob j
              liftIO (S.getJobStatus sess job)
            WaitStarted mtimeout j ->
              liftIO . S.waitStarted sess mtimeout
                =<< getJob j
            WaitTerminated mtimeout j ->
              liftIO . S.waitTerminated sess mtimeout
                =<< getJob j
            Cancel j -> liftIO . S.cancel sess =<< getJob j

runSchedulerPure ::
  forall s es a.
  (HasCallStack, S.JobScheduler s Identity) =>
  S.SchedulerConfig s ->
  Eff (Scheduler ': es) a ->
  Eff es a
runSchedulerPure cfg act =
  bracket (pure $ runIdentity $ S.newSession cfg) (pure . runIdentity . S.closeSession) $ \sess ->
    loop sess act
  where
    loop ::
      S.Session s ->
      Eff (Scheduler ': es') x ->
      Eff es' x
    loop sess =
      let getJob (Job i) = gets (IntMap.! fromIntegral i)
       in reinterpret (evalState @(IntMap (S.Job s)) mempty) \_ -> \case
            ScheduleJob spec -> do
              let rawJob = runIdentity $ S.scheduleJob sess spec
              state $ \dic ->
                case IntMap.lookupMax dic of
                  Nothing -> (Job 0, IntMap.singleton 0 rawJob)
                  Just (k, _) -> (Job $ k + 1, IntMap.insert k rawJob dic)
            GetJobInfo j -> do
              job <- getJob j
              pure $ Nothing <$ runIdentity (S.getJobInfo sess job)
            GetJobStatus j -> do
              job <- getJob j
              pure $ runIdentity $ S.getJobStatus sess job
            WaitStarted mtimeout j ->
              runIdentity . S.waitStarted sess mtimeout <$> getJob j
            WaitTerminated mtimeout j ->
              runIdentity . S.waitTerminated sess mtimeout <$> getJob j
            Cancel j ->
              runIdentity . S.cancel sess <$> getJob j
