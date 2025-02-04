{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Development.LAMMPS.GitOps.Paths.Workflow (
  repoRootDirFor,
  getRepoRootDir,
  repoJobsDir,
  getRepoJobsDir,
  getSingleJobDir,
  jobLogsDir,
  getJobLogsDir,
  jobWorkerLogFile,
  jobStderrLogFile,
  jobStdoutLogFile,
  jobCloneDir,
  getJobCloneDir,
  jobScriptFile,
  workerRepoConfig,
  getWorkerRepoConfig,
  getWorkerPidFile,
  module Development.LAMMPS.GitOps.Paths,
) where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.Reflection (Given)
import Data.Text qualified as T
import Development.LAMMPS.GitOps.Paths
import Development.LAMMPS.GitOps.Types
import Path.Tagged

repoRootDirFor :: RepoID -> PathTo RepoRootDir (RelTo CacheDir) Dir
repoRootDirFor repoID = [reldir|repos|] </> fromJust (parseRelDir $ show repoID)

getRepoRootDir :: (MonadIO f) => RepoID -> f (PathTo RepoRootDir Abs Dir)
getRepoRootDir rid = getAppCacheDir <&> (</> repoRootDirFor rid)

repoJobsDir :: PathTo RepoJobsDir (RelTo RepoRootDir) Dir
repoJobsDir = [reldir|jobs|]

getRepoJobsDir :: (MonadIO f) => RepoID -> f (PathTo RepoJobsDir Abs Dir)
getRepoJobsDir rid = getRepoRootDir rid <&> (</> repoJobsDir)

getSingleJobDir :: (MonadIO m) => RepoID -> CommitHash -> m (PathTo SingleJobDir Abs Dir)
getSingleJobDir rid (CommitHash comm) =
  (</>)
    <$> getRepoJobsDir rid
    <*> liftIO (parseRelDir $ T.unpack comm)

jobLogsDir :: PathTo JobLogsDir (RelTo SingleJobDir) Dir
jobLogsDir = [reldir|logs|]

getJobLogsDir :: (MonadIO f) => RepoID -> CommitHash -> f (PathTo JobLogsDir Abs Dir)
getJobLogsDir repo comm = getSingleJobDir repo comm <&> (</> jobLogsDir)

jobWorkerLogFile :: PathTo JobWorkerLogFile (RelTo JobLogsDir) File
jobWorkerLogFile = [relfile|worker.log|]

jobStderrLogFile :: PathTo JobStderrLogFile (RelTo JobLogsDir) File
jobStderrLogFile = [relfile|job-stderr.log|]

jobStdoutLogFile :: PathTo JobStdoutLogFile (RelTo JobLogsDir) File
jobStdoutLogFile = [relfile|job-stdout.log|]

jobCloneDir :: PathTo JobCloneDir (RelTo SingleJobDir) Dir
jobCloneDir = [reldir|work|]

getJobCloneDir :: (MonadIO m) => RepoID -> CommitHash -> m (PathTo JobCloneDir Abs Dir)
getJobCloneDir repo comm = getSingleJobDir repo comm <&> (</> jobCloneDir)

jobScriptFile :: PathTo JobScriptFile (RelTo SingleJobDir) File
jobScriptFile = [relfile|job.sh|]

getWorkerRepoConfig :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => RepoID -> m (PathTo WorkerRepoConfigYaml Abs File)
getWorkerRepoConfig repoID = do
  getAppConfigDir <&> (</> workerRepoConfig repoID)

workerRepoConfig :: RepoID -> PathTo WorkerRepoConfigYaml (RelTo ConfigDir) File
workerRepoConfig (RepoID rid) =
  [reldir|repos|] </> fromJust (parseRelFile $ show rid <> ".yaml")

workerPidFile :: PathTo WorkerPidFile (RelTo SingleJobDir) File
workerPidFile = [relfile|worker.pid|]

getWorkerPidFile :: (MonadIO m) => RepoID -> CommitHash -> m (PathTo WorkerPidFile Abs File)
getWorkerPidFile rid comm =
  getSingleJobDir rid comm <&> (</> workerPidFile)
