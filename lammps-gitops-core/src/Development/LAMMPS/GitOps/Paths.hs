{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Development.LAMMPS.GitOps.Paths (
  PathName (..),
  ConfigDir,
  getAppConfigDir,
  GitHubAppKey,
  gitHubAppKeyFile,
  getGitHubAppKeyFile,
  DaemonConfigYaml,
  daemonConfigYaml,
  getDaemonConfigYaml,
  EtcDir,
  defaultEtcDir,
  etcDir,
  JWKFile,
  jwkFile,
  getJWKFile,
  CacheDir,
  getAppCacheDir,
  DataDir,
  getAppDataDir,
  SyntaxDir,
  syntaxDir,
  SyntaxFile,
  syntaxFiles,
  DbsDir,
  dbsDir,
  getDbsDir,
  UserDbFile,
  userDbFile,
  getUserDbFile,
  GitHubDbFile,
  gitHubDbFile,
  getGitHubDbFile,
  LibDir,
  libDir,
  getAppLibDir,
  DaemonObj,
  daemonObj,
  getDaemonObj,
  WorkerObj,
  workerObj,
  getWorkerObj,
  BinDir,
  binDir,
  getAppBinDir,
  DaemonBin,
  daemonBin,
  getDaemonBin,
  ClientBin,
  clientBin,
  getClientBin,
  AdminBin,
  adminBin,
  getAdminBin,
  WorkerBin,
  workerBin,
  getWorkerBin,
  RepoJobsDir,
  SingleJobDir,
  RepoRootDir,
  JobCloneDir,
  JobLogsDir,
  JobWorkerLogFile,
  JobScriptFile,
  JobStderrLogFile,
  JobStdoutLogFile,
  ReportResource,
  ReportResourceDir,
  RepoGitOpsConfigYaml,
  repoGitOpsConfigYaml,
  ReportLogFile,
  WorkerRepoConfigYaml,
  WorkerPidFile,
) where

import Control.Monad.IO.Class (MonadIO)
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Reflection (Given, given)
import Data.Text (Text)
import Effectful.FileSystem (XdgDirectory (..))
import GHC.Generics (Generic)
import Path.Tagged
import Path.Tagged.IO (getXdgDir)

data PathName
  = EtcDir
  | ConfigDir
  | DaemonConfigYaml
  | WorkerRepoConfigYaml
  | RepoGitOpsConfigYaml
  | JWKFile
  | CacheDir
  | DataDir
  | SyntaxDir
  | SyntaxFile
  | DbsDir
  | UserDbFile
  | GitHubDbFile
  | LibDir
  | DaemonObj
  | WorkerObj
  | BinDir
  | DaemonBin
  | ClientBin
  | AdminBin
  | WorkerBin
  | RepoRootDir
  | RepoJobsDir
  | SingleJobDir
  | JobCloneDir
  | JobLogsDir
  | JobWorkerLogFile
  | JobStdoutLogFile
  | JobStderrLogFile
  | JobScriptFile
  | JobReportFile
  | ReportResource
  | ReportLogFile
  | ReportResourceDir
  | GitHubAppKey
  | WorkerPidFile
  deriving (Show, Eq, Ord, Generic)

type ConfigDir = 'ConfigDir

type EtcDir = 'EtcDir

defaultEtcDir :: PathTo 'EtcDir Abs Dir
defaultEtcDir = [absdir|/etc/gitops|]

etcDir :: (Given (PathTo 'EtcDir Abs Dir)) => PathTo 'EtcDir Abs Dir
etcDir = given @(PathTo 'EtcDir Abs Dir)

getAppConfigDir ::
  (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) =>
  m (PathTo ConfigDir Abs Dir)
getAppConfigDir = pure $ etcDir </> [reldir|config|]

type GitHubAppKey = 'GitHubAppKey

gitHubAppKeyFile :: PathTo GitHubAppKey (RelTo ConfigDir) File
gitHubAppKeyFile = [relfile|github.pem|]

getGitHubAppKeyFile ::
  (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) =>
  m (PathTo GitHubAppKey Abs File)
getGitHubAppKeyFile = getAppConfigDir <&> (</> gitHubAppKeyFile)

type DaemonConfigYaml = 'DaemonConfigYaml

daemonConfigYaml :: PathTo DaemonConfigYaml (RelTo ConfigDir) File
daemonConfigYaml = [relfile|daemon.yml|]

getDaemonConfigYaml :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo DaemonConfigYaml Abs File)
getDaemonConfigYaml = getAppConfigDir <&> (</> daemonConfigYaml)

type WorkerRepoConfigYaml = 'WorkerRepoConfigYaml

type JWKFile = 'JWKFile

jwkFile :: PathTo JWKFile (RelTo ConfigDir) File
jwkFile = [relfile|jwk.json|]

getJWKFile ::
  (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) =>
  m (PathTo JWKFile Abs File)
getJWKFile = getAppConfigDir <&> (</> jwkFile)

type CacheDir = 'CacheDir

getAppCacheDir :: (MonadIO m) => m (PathTo CacheDir Abs Dir)
getAppCacheDir = getXdgDir @'XdgCache [reldir|gitops|]

type DataDir = 'DataDir

getAppDataDir ::
  (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) =>
  m (PathTo DataDir Abs Dir)
getAppDataDir = pure $ given @(PathTo 'EtcDir Abs Dir) </> [reldir|data|]

type SyntaxDir = 'SyntaxDir

syntaxDir :: PathTo SyntaxDir (RelTo DataDir) Dir
syntaxDir = [reldir|syntax|]

type SyntaxFile = 'SyntaxFile

syntaxFiles :: Map Text (PathTo SyntaxFile (RelTo SyntaxDir) File)
syntaxFiles =
  Map.fromList
    [ ("YAML", [relfile|yaml.xml|])
    , ("TOML", [relfile|toml.xml|])
    , -- NOTE: Uses YAML definition also for JSON to avoid GPL
      ("JSON", [relfile|yaml.xml|])
    , ("Python", [relfile|python.xml|])
    ]

type DbsDir = 'DbsDir

dbsDir :: PathTo DbsDir (RelTo DataDir) Dir
dbsDir = [reldir|dbs|]

getDbsDir :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo DbsDir Abs Dir)
getDbsDir = getAppDataDir <&> (</> dbsDir)

type UserDbFile = 'UserDbFile

userDbFile :: PathTo UserDbFile (RelTo DbsDir) File
userDbFile = [relfile|users.db|]

getUserDbFile :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo UserDbFile Abs File)
getUserDbFile = getDbsDir <&> (</> userDbFile)

type GitHubDbFile = 'GitHubDbFile

gitHubDbFile :: PathTo GitHubDbFile (RelTo DbsDir) File
gitHubDbFile = [relfile|github.db|]

getGitHubDbFile :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo GitHubDbFile Abs File)
getGitHubDbFile = getDbsDir <&> (</> gitHubDbFile)

type LibDir = 'LibDir

libDir :: PathTo LibDir (RelTo DataDir) Dir
libDir = [reldir|lib|]

getAppLibDir ::
  (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) =>
  m (PathTo LibDir Abs Dir)
getAppLibDir = pure $ etcDir </> [reldir|lib|]

type BinDir = 'BinDir

binDir :: PathTo BinDir (RelTo DataDir) Dir
binDir = [reldir|bin|]

getAppBinDir :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo BinDir Abs Dir)
getAppBinDir = pure $ etcDir </> [reldir|bin|]

type DaemonBin = 'DaemonBin

daemonBin :: PathTo DaemonBin (RelTo BinDir) File
daemonBin = [relfile|lammopsd|]

getDaemonBin :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo DaemonBin Abs File)
getDaemonBin = getAppBinDir <&> (</> daemonBin)

type ClientBin = 'ClientBin

clientBin :: PathTo ClientBin (RelTo BinDir) File
clientBin = [relfile|lammops-cli|]

getClientBin :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo ClientBin Abs File)
getClientBin = getAppBinDir <&> (</> clientBin)

type AdminBin = 'AdminBin

adminBin :: PathTo AdminBin (RelTo BinDir) File
adminBin = [relfile|lammops-admin|]

getAdminBin :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo AdminBin Abs File)
getAdminBin = getAppBinDir <&> (</> adminBin)

type WorkerBin = 'WorkerBin

workerBin :: PathTo WorkerBin (RelTo BinDir) File
workerBin = [relfile|lammops-worker|]

getWorkerBin :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo WorkerBin Abs File)
getWorkerBin = getAppBinDir <&> (</> workerBin)

type DaemonObj = 'DaemonObj

daemonObj :: PathTo DaemonObj (RelTo LibDir) File
daemonObj = [relfile|lammopsd.so|]

getDaemonObj :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo DaemonObj Abs File)
getDaemonObj = getAppLibDir <&> (</> daemonObj)

type WorkerObj = 'WorkerObj

workerObj :: PathTo WorkerObj (RelTo LibDir) File
workerObj = [relfile|lammops-worker.so|]

getWorkerObj :: (MonadIO m, Given (PathTo 'EtcDir Abs Dir)) => m (PathTo WorkerObj Abs File)
getWorkerObj = getAppLibDir <&> (</> workerObj)

type RepoJobsDir = 'RepoJobsDir

type SingleJobDir = 'SingleJobDir

type RepoRootDir = 'RepoRootDir

type JobCloneDir = 'JobCloneDir

type JobLogsDir = 'JobLogsDir

type JobWorkerLogFile = 'JobWorkerLogFile

type JobScriptFile = 'JobScriptFile

type JobStdoutLogFile = 'JobStdoutLogFile

type JobStderrLogFile = 'JobStderrLogFile

type ReportResource = 'ReportResource

type ReportLogFile = 'ReportLogFile

type ReportResourceDir = 'ReportResourceDir

type RepoGitOpsConfigYaml = 'RepoGitOpsConfigYaml

repoGitOpsConfigYaml :: PathTo RepoGitOpsConfigYaml (RelTo JobCloneDir) File
repoGitOpsConfigYaml = [relfile|.gitops.yaml|]

type WorkerPidFile = 'WorkerPidFile
