{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.Worker.Types (
  WorkerEnv (..),
  GitOpsJobSource (..),
  WorkerConfig (..),
  ReportConfig (..),
  RepoConfig (..),
  StorageConfig (..),
  GitHubConfig (..),
  withGitHubDb,
  WorkerException (..),
  viewClonedDir,
  SchedulerType (..),
  runRepoAPI,
) where

import Control.Concurrent.STM.TMVar (TMVar)
import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON)
import Data.Generics.Labels ()
import Data.Text (Text)
import Development.LAMMPS.GitOps.Paths
import Development.LAMMPS.GitOps.Types
import Development.LAMMPS.GitOps.Workflow.Config
import Effectful
import Effectful.Concurrent.TimedResource (Expiration)
import Effectful.Database.Beam.Sqlite
import Effectful.Network.GitHub.Apps (APITokens, GitHub, GitHubRepo, Repository, runGitHubWith, withGitHubRepo)
import Effectful.Network.Http (Http)
import Effectful.Reader.Static
import Effectful.Reader.Static.Lens qualified as EffL
import GHC.Generics
import Path.Tagged (Abs, Dir, PathTo)

viewClonedDir ::
  (Reader WorkerEnv :> es) =>
  Eff es (PathTo JobCloneDir Abs Dir)
viewClonedDir = EffL.view #clonedDir

data WorkerConfig = WorkerConfig
  { scheduler :: !SchedulerType
  , github :: !GitHubConfig
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data WorkerEnv = WorkerEnv
  { repo :: !RepoID
  , repoName :: !Repository
  , commit :: !CommitHash
  , pullNumber :: !IssueNumber
  , branch :: !Text
  , commitMessage :: !Text
  , jobUser :: !Text
  , workerConfig :: !WorkerConfig
  , repoConfig :: !RepoConfig
  , githubDbPool :: !SqlitePool
  , workflow :: !Workflow
  , toolchain :: !Toolchain
  , clonedDir :: !(PathTo JobCloneDir Abs Dir)
  , apiToken :: !APITokens
  , cancelSwitch :: !(TMVar ())
  }
  deriving (Generic)

data WorkerException = NoContextFound !RepoID !CommitHash
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

withGitHubDb ::
  (Reader WorkerEnv :> es, IOE :> es) =>
  Eff (Sqlite ': es) a ->
  Eff es a
withGitHubDb act = do
  pool <- EffL.view #githubDbPool
  runSqlite (DbPool pool) act

runRepoAPI ::
  (Http :> es, Expiration :> es, Reader WorkerEnv :> es) =>
  Eff (GitHubRepo ': GitHub ': es) a ->
  Eff es a
runRepoAPI act = do
  apiTok <- EffL.view #apiToken
  repo <- EffL.view #repoName
  runGitHubWith apiTok $ withGitHubRepo repo act
