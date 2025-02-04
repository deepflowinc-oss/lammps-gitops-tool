{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Development.LAMMPS.GitOps.Types (
  RepoID (..),
  OwnerID (..),
  IssueNumber (..),
  U.MVector (MV_RepoID, MV_OwnerID, MV_IssueNumber),
  U.Vector (V_RepoID, V_OwnerID, V_IssueNumber),
  RepoName (..),
  PrimaryKey (..),
  parseRepo,
  RepoF (..),
  Repo,
  selectRepoNamed,
  GitOpsJobSource (..),
  PagesConfig (..),
  FrontendUrl (..),
  PullRequestF (..),
  PullRequest,
  JobF (..),
  Job,
  CommitF (..),
  Commit,
  toJobUpdate,
  lookupJob,
  JobStatus (..),
  convertJobStatus,
  CommitHash (..),
  GitHubDb (..),
  gitHubDb,
  gitHubDbChecked,
  migrateGitHubDb,
  FullContextF (..),
  selectFullContext,
  GitHubConfig (..),
  RepoConfig (..),
  dummyRepoConfig,
  ReportConfig (..),
  StorageConfig (..),
) where

import Control.DeepSeq (NFData)
import Control.Exception.Safe (throwM)
import Control.Lens (view, (^.))
import Control.Monad (guard)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Coerce (coerce)
import Data.Generics.Labels ()
import Data.Hashable (Hashable)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (fromJust)
import Data.Reflection (Given)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector.Generic qualified as G
import Data.Vector.Generic.Mutable qualified as MV
import Data.Vector.Unboxed qualified as U
import Data.Word (Word64)
import Data.Yaml (FromJSON (..))
import Database.Beam
import Database.Beam qualified as Beam
import Database.Beam.Backend
import Database.Beam.Backend qualified as Beam
import Database.Beam.Migrate hiding (p)
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Sqlite qualified as Sqlite
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Database.Beam.Sqlite.Syntax qualified as Sqlite
import Database.SQLite.Simple.FromField qualified as Sqlite
import Database.SQLite.Simple.ToField qualified as Sqlite
import Database.Utils
import Development.Job.Scheduler.Class qualified as JS
import Development.LAMMPS.GitOps.Paths (EtcDir, getGitHubDbFile)
import Effectful (Eff, IOE, (:>))
import Effectful.Concurrent
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.Fail
import Effectful.FileSystem (runFileSystem)
import Effectful.FileSystem.Tagged (createDirIfMissing)
import Effectful.Log
import Effectful.Network.GitHub.Apps (AppID, Repository)
import Effectful.Random.Static
import Network.S3.Sign.V4 (S3Config (..), S3Endpoint (..), S3Keys (..))
import Network.URI (URI, parseURI)
import Network.URI.Orphans ()
import Path.Tagged (Abs, Dir, PathTo, absdir, fromAbsFile, parent, untagPath)
import Servant.API (FromHttpApiData, ToHttpApiData)

parseRepo :: String -> Maybe RepoName
parseRepo str = do
  let (repoOwner, T.drop 1 -> repoName) = T.breakOn "/" $ T.pack str
  guard $ not $ T.null repoName
  pure RepoName {..}

data GitHubDb f = GitHubDb
  { repos :: !(f (TableEntity RepoF))
  , pulls :: !(f (TableEntity PullRequestF))
  , commits :: !(f (TableEntity CommitF))
  , jobs :: !(f (TableEntity JobF))
  }
  deriving (Generic)
  deriving anyclass (Database be)

gitHubDb :: DatabaseSettings Sqlite.Sqlite GitHubDb
gitHubDb = unCheckDatabase gitHubDbChecked

migrateGitHubDb ::
  (IOE :> es, Log :> es, Random :> es, Given (PathTo EtcDir Abs Dir)) =>
  Eff es ()
migrateGitHubDb = runConcurrent $ localDomain "migrate-gh" $ do
  logInfo_ "Migrating GitHub Db..."
  dbPath <- liftIO getGitHubDbFile
  runFileSystem $ createDirIfMissing True $ parent dbPath
  logInfo_ $ "GitHub DB Path: " <> T.pack (fromAbsFile dbPath)
  Db.runSqliteDebug (Db.DbFile $ untagPath dbPath) $ do
    either (throwM . userError) pure
      =<< runFail (Db.transactExclusive $ Db.liftSqliteM $ autoMigrate migrationBackend gitHubDbChecked)
  logInfo_ "Migration done!"

instance (BeamMigrateSqlBackend be) => HasDefaultSqlDataType be UTCTime where
  defaultSqlDataType _ _ _ = timeType Nothing False

gitHubDbChecked :: CheckedDatabaseSettings Sqlite.Sqlite GitHubDb
gitHubDbChecked =
  defaultMigratableDbSettings
    `withDbModification` GitHubDb
      { repos =
          modifyCheckedTable
            id
            Repo {repoID = "repoID", repoName = "repoName", ownerName = "ownerName", description = "description"}
      , pulls =
          modifyCheckedTable
            id
            PullRequest
              { repo = RepoKey "repo"
              , pullNumber = "pullNumber"
              , branch = "branch"
              , title = "title"
              , author = "author"
              , description = "description"
              }
      , commits =
          modifyCheckedTable
            id
            Commit
              { repo = RepoKey "repo"
              , commit = "commit"
              , pull = "pullNumber"
              , createdAt = "createdAt"
              , cancelled = "cancelled"
              }
      , jobs =
          modifyCheckedTable
            id
            Job
              { commit = CommitKey (RepoKey "repo") "commit"
              , commitMessage = "commitMessage"
              , status = "status"
              , pullRequest = "pullRequest"
              , jobName = "jobName"
              , jobId = "jobId"
              , user = "user"
              , scheduledAt = "scheduledAt"
              , startedAt = "startedAt"
              , finishedAt = "finishedAt"
              }
      }

newtype CommitHash = CommitHash {getCommitHash :: T.Text}
  deriving (Generic)
  deriving newtype (Show, Eq, Ord, IsString, Hashable, NFData)
  deriving newtype (ToJSON, FromJSON)
  deriving newtype (FromHttpApiData, ToHttpApiData)
  deriving newtype
    ( Sqlite.FromField
    , Sqlite.ToField
    , HasDefaultSqlDataType Sqlite.Sqlite
    , HasSqlValueSyntax Sqlite.SqliteValueSyntax
    , HasSqlEqualityCheck Sqlite.Sqlite
    , Beam.FromBackendRow Sqlite.Sqlite
    , Beam.BeamSqlBackendIsString be
    )

newtype RepoID = RepoID {runRepoID :: Word64}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, Num, Integral, Real, Enum)
  deriving newtype (FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
  deriving newtype (Hashable, NFData)
  deriving newtype (U.Unbox, MV.MVector U.MVector, G.Vector U.Vector)
  deriving newtype
    ( Sqlite.FromField
    , Sqlite.ToField
    , HasDefaultSqlDataType Sqlite.Sqlite
    , HasSqlValueSyntax Sqlite.SqliteValueSyntax
    , HasSqlEqualityCheck Sqlite.Sqlite
    , Beam.FromBackendRow Sqlite.Sqlite
    )

newtype instance U.MVector s RepoID = MV_RepoID (U.MVector s Word64)

newtype instance U.Vector RepoID = V_RepoID (U.Vector Word64)

newtype OwnerID = OwnerID {runOwnerID :: Word64}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, Num, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
  deriving newtype (Hashable, NFData)
  deriving newtype (U.Unbox, MV.MVector U.MVector, G.Vector U.Vector)
  deriving newtype
    ( Sqlite.FromField
    , Sqlite.ToField
    , HasDefaultSqlDataType Sqlite.Sqlite
    , HasSqlValueSyntax Sqlite.SqliteValueSyntax
    , HasSqlEqualityCheck Sqlite.Sqlite
    , Beam.FromBackendRow Sqlite.Sqlite
    )

newtype instance U.MVector s OwnerID = MV_OwnerID (U.MVector s Word64)

newtype instance U.Vector OwnerID = V_OwnerID (U.Vector Word64)

newtype IssueNumber = IssueNumber {getIssueNumber :: Word64}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, Read, Enum)
  deriving newtype (Num, Integral, Real, FromJSON, ToJSON, FromHttpApiData, ToHttpApiData)
  deriving newtype (Hashable, NFData)
  deriving newtype (U.Unbox, MV.MVector U.MVector, G.Vector U.Vector)
  deriving newtype
    ( Sqlite.FromField
    , Sqlite.ToField
    , HasDefaultSqlDataType Sqlite.Sqlite
    , HasSqlValueSyntax Sqlite.SqliteValueSyntax
    , HasSqlEqualityCheck Sqlite.Sqlite
    , Beam.FromBackendRow Sqlite.Sqlite
    )

newtype instance U.MVector s IssueNumber = MV_IssueNumber (U.MVector s Word64)

newtype instance U.Vector IssueNumber = V_IssueNumber (U.Vector Word64)

data RepoName = RepoName {repoOwner, repoName :: {-# UNPACK #-} !T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable, NFData)

type Repo = RepoF Identity

data RepoF f = Repo
  { repoID :: !(Columnar f RepoID)
  , repoName :: !(Columnar f Text)
  , ownerName :: !(Columnar f Text)
  , description :: !(Columnar f Text)
  }
  deriving (Generic)
  deriving anyclass (Beamable)

selectRepoNamed :: RepoName -> Q Sqlite.Sqlite GitHubDb s (RepoF (QExpr Sqlite.Sqlite s))
selectRepoNamed RepoName {..} = do
  Beam.filter_
    ( \r ->
        (r ^. #repoName ==. val_ repoName)
          &&. (r ^. #ownerName ==. val_ repoOwner)
    )
    $ Beam.all_ (gitHubDb ^. #repos)

deriving instance
  ( Show (Columnar f RepoID)
  , Show (Columnar f Text)
  ) =>
  Show (RepoF f)

deriving instance
  ( Eq (Columnar f RepoID)
  , Eq (Columnar f Text)
  ) =>
  Eq (RepoF f)

deriving instance
  ( Ord (Columnar f RepoID)
  , Ord (Columnar f Text)
  ) =>
  Ord (RepoF f)

deriving newtype instance
  (Show (Columnar f RepoID)) => Show (PrimaryKey RepoF f)

deriving newtype instance Num (PrimaryKey RepoF Identity)

deriving instance
  (Eq (Columnar f RepoID)) => Eq (PrimaryKey RepoF f)

deriving instance
  (Ord (Columnar f RepoID)) => Ord (PrimaryKey RepoF f)

deriving newtype instance
  (NFData (Columnar f RepoID)) => NFData (PrimaryKey RepoF f)

deriving newtype instance
  (Hashable (Columnar f RepoID)) => Hashable (PrimaryKey RepoF f)

instance Table RepoF where
  newtype PrimaryKey RepoF f = RepoKey (Columnar f RepoID)
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = RepoKey . view #repoID
  {-# INLINE primaryKey #-}

deriving newtype instance ToJSON (PrimaryKey RepoF Identity)

deriving newtype instance FromJSON (PrimaryKey RepoF Identity)

data GitOpsJobSource = GitOpsJobSource
  { repo :: !RepoID
  , repoName :: !Repository
  , commit :: !CommitHash
  , pullNumber :: !IssueNumber
  , user :: !Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData, Hashable)

newtype FrontendUrl = FrontendUrl {getFrontendUrl :: T.Text}
  deriving (Eq, Ord, Generic)
  deriving newtype (Show, NFData, Hashable)

instance IsString FrontendUrl where
  fromString = parseFrontendUrl . T.pack

instance FromJSON FrontendUrl where
  parseJSON = J.withText "url" $ pure . parseFrontendUrl

instance ToJSON FrontendUrl where
  toJSON = J.toJSON . view #getFrontendUrl

parseFrontendUrl :: T.Text -> FrontendUrl
parseFrontendUrl str =
  if "/" `T.isSuffixOf` str
    then FrontendUrl str
    else FrontendUrl $ str <> "/"

data PagesConfig = PagesConfig
  { endpoint :: URI
  , region :: Maybe T.Text
  , bucket :: T.Text
  , frontendUrl :: T.Text
  , accessKeyId :: T.Text
  , accessKeySecret :: T.Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, Hashable, NFData)

convertJobStatus :: JS.JobStatus -> JobStatus
convertJobStatus JS.Queued = Queued
convertJobStatus JS.Held = Held
convertJobStatus JS.Cancelled {} = Cancelled
convertJobStatus JS.Suspended = Held
convertJobStatus JS.Running = Running
convertJobStatus JS.Success = Successed
convertJobStatus JS.Aborted = Aborted
convertJobStatus JS.Errored {} = Aborted

data JobStatus = Held | Queued | Running | Successed | Aborted | Cancelled
  deriving (Show, Read, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (NFData, Hashable)
  deriving anyclass (FromJSON, ToJSON)
  deriving (Sqlite.FromField, Beam.FromBackendRow Sqlite.Sqlite) via Reading JobStatus
  deriving (Sqlite.ToField) via Shown JobStatus

instance HasDefaultSqlDataType Sqlite.Sqlite JobStatus where
  defaultSqlDataType _ _ _ = varCharType Nothing Nothing

instance HasSqlValueSyntax Sqlite.SqliteValueSyntax JobStatus where
  sqlValueSyntax = sqlValueSyntax . T.pack . show

deriving anyclass instance
  HasSqlEqualityCheck Sqlite.Sqlite JobStatus

data PullRequestF f = PullRequest
  { repo :: !(PrimaryKey RepoF f)
  , pullNumber :: !(Columnar f IssueNumber)
  , branch :: !(Columnar f Text)
  , title :: !(Columnar f Text)
  , author :: !(Columnar f Text)
  , description :: !(Columnar f Text)
  }
  deriving (Generic)
  deriving anyclass (Beamable)

instance Table PullRequestF where
  data PrimaryKey PullRequestF f = PullReqKey
    { repo :: !(PrimaryKey RepoF f)
    , pullNumber :: !(Columnar f IssueNumber)
    }
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = PullReqKey <$> view #repo <*> view #pullNumber
  {-# INLINE primaryKey #-}

deriving anyclass instance ToJSON (PrimaryKey PullRequestF Identity)

deriving anyclass instance FromJSON (PrimaryKey PullRequestF Identity)

deriving instance
  (Show (Columnar f IssueNumber), Show (PrimaryKey RepoF f)) =>
  Show (PrimaryKey PullRequestF f)

deriving instance
  (Eq (Columnar f IssueNumber), Eq (PrimaryKey RepoF f)) =>
  Eq (PrimaryKey PullRequestF f)

deriving instance
  (Ord (Columnar f IssueNumber), Ord (PrimaryKey RepoF f)) =>
  Ord (PrimaryKey PullRequestF f)

deriving anyclass instance (Hashable (Columnar f IssueNumber), Hashable (PrimaryKey RepoF f)) => Hashable (PrimaryKey PullRequestF f)

deriving anyclass instance (NFData (Columnar f IssueNumber), NFData (PrimaryKey RepoF f)) => NFData (PrimaryKey PullRequestF f)

type PullRequest = PullRequestF Identity

deriving instance Show PullRequest

deriving instance Eq PullRequest

deriving instance Ord PullRequest

deriving anyclass instance Hashable PullRequest

deriving anyclass instance NFData PullRequest

deriving anyclass instance FromJSON PullRequest

deriving anyclass instance ToJSON PullRequest

lookupJob ::
  RepoName ->
  CommitHash ->
  Either String (Q Sqlite.Sqlite GitHubDb s (JobF (QExpr Sqlite.Sqlite s)))
lookupJob repo hash
  | len > 40 =
      Left $
        "Commit hash must be <= 40 digits, but got "
          <> show len
          <> ": "
          <> T.unpack (coerce hash)
  | len == 40 = Right $ do
      r <- selectRepoNamed repo
      Beam.related_ (gitHubDb ^. #jobs) $ JobKey $ CommitKey (RepoKey $ r ^. #repoID) (val_ hash)
  | otherwise = Right $ do
      r <- selectRepoNamed repo
      Beam.filter_
        ( \j ->
            (j ^. #commit . #repo ==. RepoKey (r ^. #repoID))
              &&. (j ^. #commit . #commit `Beam.like_` val_ (CommitHash $ (hash ^. #getCommitHash) <> "%"))
        )
        $ Beam.all_ (gitHubDb ^. #jobs)
  where
    len = T.length $ coerce hash

data CommitF f = Commit
  { repo :: !(PrimaryKey RepoF f)
  , commit :: !(Columnar f CommitHash)
  , pull :: !(Columnar f IssueNumber)
  , createdAt :: !(Columnar f UTCTime)
  , cancelled :: !(Columnar f Bool)
  }
  deriving (Generic)
  deriving anyclass (Beamable)

type Commit = CommitF Identity

deriving instance Show Commit

deriving instance Eq Commit

deriving instance Ord Commit

deriving anyclass instance Hashable Commit

deriving anyclass instance NFData Commit

deriving anyclass instance ToJSON Commit

deriving anyclass instance FromJSON Commit

instance Table CommitF where
  data PrimaryKey CommitF f = CommitKey
    { repo :: !(PrimaryKey RepoF f)
    , commit :: !(Columnar f CommitHash)
    }
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = CommitKey <$> view #repo <*> view #commit
  {-# INLINE primaryKey #-}

deriving instance
  ( Show (PrimaryKey RepoF f)
  , Show (Columnar f CommitHash)
  ) =>
  Show (PrimaryKey CommitF f)

deriving instance
  ( Eq (PrimaryKey RepoF f)
  , Eq (Columnar f CommitHash)
  ) =>
  Eq (PrimaryKey CommitF f)

deriving instance
  ( Ord (PrimaryKey RepoF f)
  , Ord (Columnar f CommitHash)
  ) =>
  Ord (PrimaryKey CommitF f)

deriving anyclass instance
  ( NFData (PrimaryKey RepoF f)
  , NFData (Columnar f CommitHash)
  ) =>
  NFData (PrimaryKey CommitF f)

deriving anyclass instance
  ( Hashable (PrimaryKey RepoF f)
  , Hashable (Columnar f CommitHash)
  ) =>
  Hashable (PrimaryKey CommitF f)

deriving anyclass instance
  ( ToJSON (PrimaryKey RepoF f)
  , ToJSON (Columnar f CommitHash)
  ) =>
  ToJSON (PrimaryKey CommitF f)

deriving anyclass instance
  ( FromJSON (PrimaryKey RepoF f)
  , FromJSON (Columnar f CommitHash)
  ) =>
  FromJSON (PrimaryKey CommitF f)

data JobF f = Job
  { commit :: !(PrimaryKey CommitF f)
  , commitMessage :: !(Columnar f Text)
  , status :: !(Columnar f JobStatus)
  , pullRequest :: !(Columnar f IssueNumber)
  , jobName :: !(Columnar f T.Text)
  , jobId :: !(Columnar f T.Text)
  , user :: !(Columnar f T.Text)
  , scheduledAt :: !(Columnar f UTCTime)
  , startedAt :: !(Columnar (Nullable f) UTCTime)
  , finishedAt :: !(Columnar (Nullable f) UTCTime)
  }
  deriving (Generic)
  deriving anyclass (Beamable)

type Job = JobF Identity

deriving instance Show Job

deriving instance Eq Job

deriving instance Ord Job

deriving anyclass instance Hashable Job

deriving anyclass instance NFData Job

deriving anyclass instance ToJSON Job

deriving anyclass instance FromJSON Job

instance Table JobF where
  newtype PrimaryKey JobF f = JobKey (PrimaryKey CommitF f)
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = JobKey . view #commit
  {-# INLINE primaryKey #-}

deriving instance Show (PrimaryKey JobF Identity)

deriving instance Eq (PrimaryKey JobF Identity)

deriving instance Ord (PrimaryKey JobF Identity)

deriving newtype instance NFData (PrimaryKey JobF Identity)

deriving newtype instance Hashable (PrimaryKey JobF Identity)

toJobUpdate :: RepoID -> CommitHash -> JS.JobInfo s -> SqlUpdate Sqlite.Sqlite JobF
toJobUpdate repoID comm jinfo =
  updateTable
    gitHubDb.jobs
    Job
      { commit = CommitKey (RepoKey toOldValue) toOldValue
      , commitMessage = toOldValue
      , status = toNewValue $ val_ $ convertJobStatus jinfo.jobStatus
      , pullRequest = toOldValue
      , jobName = toOldValue
      , jobId = toOldValue
      , user = toOldValue
      , scheduledAt = toNewValue $ val_ jinfo.submittedAt
      , startedAt = toNewValue $ val_ jinfo.startedAt
      , finishedAt = toNewValue $ val_ jinfo.finishedAt
      }
    (\j -> j.commit ==. CommitKey (RepoKey $ val_ repoID) (val_ comm))

data FullContextF f = FullContext
  { repo :: RepoF f
  , pull :: PullRequestF f
  , job :: JobF f
  }
  deriving (Generic)
  deriving anyclass (Beamable)

selectFullContext :: RepoID -> CommitHash -> SqlSelect Sqlite.Sqlite (FullContextF Identity)
selectFullContext repoID comm = Beam.select $ do
  repo <- Beam.related_ gitHubDb.repos $ Beam.val_ $ RepoKey repoID
  job <-
    Beam.related_ gitHubDb.jobs $ JobKey $ CommitKey (RepoKey repo.repoID) $ Beam.val_ comm
  pull <- Beam.related_ gitHubDb.pulls $ PullReqKey (RepoKey repo.repoID) job.pullRequest
  pure FullContext {..}

data GitHubConfig = GitHubConfig
  { appName :: Maybe Text
  , appID :: !AppID
  , ownerID :: !Int
  , repos :: NonEmpty Repository
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data RepoConfig = RepoConfig
  { report :: !ReportConfig
  , storage :: !StorageConfig
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ReportConfig = ReportConfig
  { s3 :: !S3Config
  , bucket :: !Text
  , bucketSubdir :: !(Maybe Text)
  , baseUrl :: !URI
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data StorageConfig
  = S3Storage
      { s3 :: !S3Config
      , bucket :: !Text
      , bucketSubdir :: !(Maybe Text)
      }
  | LocalStorage !(PathTo "Storage" Abs Dir)
  deriving (Show, Eq, Ord, Generic)

storageCfg :: J.Options
storageCfg =
  J.defaultOptions
    { J.sumEncoding = J.UntaggedValue
    }

instance FromJSON StorageConfig where
  parseJSON = J.genericParseJSON storageCfg

instance ToJSON StorageConfig where
  toJSON = J.genericToJSON storageCfg

dummyRepoConfig :: RepoConfig
dummyRepoConfig =
  RepoConfig
    { report =
        ReportConfig
          { s3 =
              S3Config
                { s3Endpoint = S3Endpoint $ fromJust $ parseURI "https://s3.example.com"
                , region = "auto"
                , keys = S3Keys {accessKeySecret = "<S3 access key secret>", accessKeyId = "<S3 Access Key Id>"}
                }
          , baseUrl = fromJust $ parseURI "https://report.example.com"
          , bucket = "gitops-reports"
          , bucketSubdir = Just "myrepo"
          }
    , storage = LocalStorage [absdir|/path/to/storage/|]
    }
