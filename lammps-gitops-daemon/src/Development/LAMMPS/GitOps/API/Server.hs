{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.API.Server (
  apiServer,
  APIServerEnv (..),
  apiRoutes,
  deployRoutes,
  repoRoutes,
  commitApi,
) where

import Control.Exception.Safe (throwM)
import Control.Lens hiding (au)
import Control.Monad (unless)
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Generics.Labels ()
import Data.Reflection (Given)
import Data.Text (Text)
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Database.Beam ((&&.), (==.))
import Database.Beam qualified as Beam
import Development.LAMMPS.GitOps.API.Types
import Development.LAMMPS.GitOps.App.Daemon.Model (verifyAuthUser)
import Development.LAMMPS.GitOps.Deployment
import Development.LAMMPS.GitOps.Paths (EtcDir)
import Development.LAMMPS.GitOps.Types
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Database.Beam.Sqlite (SqlitePool)
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.FileSystem (FileSystem)
import Effectful.Log (Log, Logger, localDomain, logInfo, logInfo_)
import Effectful.Log.Extra (logTrace_)
import Effectful.Random.Static (Random, evalRandom, newStdGen)
import Effectful.Reader.Static (Reader)
import Effectful.Reader.Static.Lens qualified as EffL
import GHC.Generics (Generic)
import Path.Tagged (Abs, Dir, PathTo)
import Servant.API hiding (inject, (:>))
import Servant.Auth.Server
import Servant.Server
import Servant.Server.Generic
import Streaming (hoist)
import Streaming.ByteString qualified as Q
import Text.Show.Extra (tshow)

data APIServerEnv = APIServerEnv
  { logger :: !Logger
  , userDbPool :: !SqlitePool
  , githubDbPool :: !SqlitePool
  , pushDeploy :: !Bool
  }
  deriving (Generic)

runGitHubDb ::
  (Reader APIServerEnv :> es, IOE :> es) =>
  Eff (Db.Sqlite ': es) a ->
  Eff es a
runGitHubDb act = do
  pool <- EffL.view #githubDbPool
  Db.runSqlite (Db.DbPool pool) act

apiServer ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , Concurrent :> es
  , Random :> es
  , FileSystem :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  ServerT (ToServantApi RestApi) (Eff es)
apiServer = genericServerT apiRoutes

apiRoutes ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , Concurrent :> es
  , Random :> es
  , FileSystem :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  RestApi (AsServerT (Eff es))
apiRoutes = RestApi {deployApi = deployRoutes, repoApi = repoRoutes}

repoRoutes ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , Concurrent :> es
  , Random :> es
  ) =>
  Text ->
  Text ->
  RepoApi (AsServerT (Eff es))
repoRoutes repoOwner repoName =
  let repo = RepoName {..}
   in RepoApi
        { listAllJobs = listAllJobs repo
        , pullApi = pullApi repo
        , commitApi = commitApi repo
        }

commitApi ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , Concurrent :> es
  , Random :> es
  ) =>
  RepoName ->
  CommitHash ->
  CommitApi (AsServerT (Eff es))
commitApi repo commitHash =
  CommitApi
    { scheduleCommitJob = scheduleCommitM repo commitHash
    , getCommitJob = getCommitJobM repo commitHash
    , cancelCommitJob = cancelCommitJobM repo commitHash
    }

pullApi ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , Concurrent :> es
  , Random :> es
  ) =>
  RepoName ->
  IssueNumber ->
  PullApi (AsServerT (Eff es))
pullApi repo pullID =
  PullApi
    { schedulePullNewestCommitJob = schedulePullJobM repo pullID
    , cancelAllPullJobs = cancellPullJobsM repo pullID
    , cancelMostRecentPullJob = cancellMostRecentPullJobM repo pullID
    , listPullJobs = getPullJobs repo pullID
    }

deployRoutes ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , FileSystem :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  DeployApi (AsServerT (Eff es))
deployRoutes = DeployApi {deploy}

deploy ::
  forall es.
  ( IOE :> es
  , Log :> es
  , Reader APIServerEnv :> es
  , FileSystem :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  AuthResult AuthUser ->
  Q.ByteStream IO () ->
  Eff es NoContent
deploy (Authenticated au@AuthUser {..}) _stream = localDomain "deploy" $ do
  deployEnabled <- EffL.view #pushDeploy
  logInfo_ $ "deployEnabled: " <> tshow deployEnabled
  unless deployEnabled $
    throwM err403 {errBody = "Deploy API is disabled"}
  logInfo_ "Deployment: verifying JWT key..."
  logInfo "JWT signature verified: " au
  pool <- EffL.view #userDbPool
  stdgen <- newStdGen
  eith <-
    runConcurrent $
      evalRandom stdgen $
        Db.runSqlite (Db.DbPool pool) $
          verifyAuthUser au
  case eith of
    Right _au' -> do
      logInfo_ $ "deployM(OK): successfully authenticated as " <> name
      dep <- liftIO defaultDeployment
      deployFrom dep $ hoist liftIO _stream

      pure NoContent
    Left err -> do
      logInfo_ "Nonce expired"
      throwM err401 {errBody = "Token expired because: " <> LBS.fromStrict (T.encodeUtf8 (tshow err))}
deploy depUsr _ = do
  logInfo_ ("deployM(user): Auth failed: " <> tshow depUsr)
  throwM err401 {errBody = "Invalid Bearer token"}

listAllJobs ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , Concurrent :> es
  , Random :> es
  ) =>
  RepoName ->
  Eff es [JobDescriptor]
listAllJobs repo = localDomain "listAllJobs" $ do
  logTrace_ $ "Looking for all jobs for: " <> tshow repo
  fmap (map $ uncurry JobDescriptor) $
    runGitHubDb $
      Db.notransact $
        Db.selectMany $
          Beam.select $
            do
              r <- selectRepoNamed repo
              job <-
                Beam.filter_
                  (\j -> j.commit.repo ==. RepoKey (r ^. #repoID))
                  $ Beam.all_ (gitHubDb ^. #jobs)
              pull <-
                Beam.related_
                  (gitHubDb ^. #pulls)
                  (PullReqKey job.commit.repo $ job ^. #pullRequest)
              pure (pull, job)

getPullJobs ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , Concurrent :> es
  , Random :> es
  ) =>
  RepoName ->
  IssueNumber ->
  Eff es [JobDescriptor]
getPullJobs repo pullNum = localDomain "getPullJobsM" $ do
  logInfo_ $ "Searching for: " <> tshow (repo, pullNum)
  logTrace_ $ "Looking for all jobs for: " <> tshow repo
  fmap (map $ uncurry JobDescriptor) $
    runGitHubDb $
      Db.notransact $
        Db.selectMany $
          Beam.select $
            do
              r <- selectRepoNamed repo
              pull <-
                Beam.filter_
                  ( \pr ->
                      pr ^. #repo ==. RepoKey (r ^. #repoID)
                  )
                  $ Beam.all_ (gitHubDb ^. #pulls)
              job <-
                Beam.filter_
                  ( \j ->
                      (j.commit.repo ==. RepoKey (r ^. #repoID))
                        &&. (j ^. #pullRequest ==. pull ^. #pullNumber)
                  )
                  $ Beam.all_ (gitHubDb ^. #jobs)
              pure (pull, job)

cancellPullJobsM :: (Log :> es) => RepoName -> IssueNumber -> Eff es [JobDescriptor]
cancellPullJobsM repo pullID =
  [] <$ logInfo_ ("cancellPullJobsM: " <> tshow (repo, pullID))

cancellMostRecentPullJobM :: (Log :> es) => RepoName -> IssueNumber -> Eff es JobDescriptor
cancellMostRecentPullJobM repo pullID = do
  logInfo_ ("cancellMostRecentPullJobM: " <> tshow (repo, pullID))
  throwM err404

schedulePullJobM :: (Log :> es) => RepoName -> IssueNumber -> Eff es JobDescriptor
schedulePullJobM repo pullId = do
  logInfo_ ("scheduleCommitJobM: " <> tshow (repo, pullId))
  throwM err501

cancelCommitJobM :: (Log :> es) => RepoName -> CommitHash -> Eff es JobDescriptor
cancelCommitJobM repo commit = do
  logInfo_ ("cancelCommitJobM: " <> tshow (repo, commit))
  throwM err404 {errBody = "No job found for commit: " <> LBS.fromStrict (T.encodeUtf8 $ coerce commit)}

scheduleCommitM :: (Log :> es) => RepoName -> CommitHash -> IssueNumber -> Eff es JobDescriptor
scheduleCommitM repo commit src = do
  logInfo_ ("scheduleCommitJobM: " <> tshow (repo, commit, src))
  throwM err501

getCommitJobM ::
  ( Log :> es
  , Reader APIServerEnv :> es
  , IOE :> es
  , Concurrent :> es
  , Random :> es
  ) =>
  RepoName ->
  CommitHash ->
  Eff es JobDescriptor
getCommitJobM repo commit = localDomain "getCommitJobM" $ do
  logInfo_ $
    "Looking up jobs for commit "
      <> coerce commit
      <> " or repo "
      <> tshow repo
  jobQ <-
    either
      ( \err ->
          throwM
            err400
              { errBody =
                  "Invalid commit: " <> LT.encodeUtf8 (LT.pack err)
              }
      )
      pure
      $ lookupJob repo commit
  mpulljob <- runGitHubDb $ Db.notransact $ Db.selectFirst $ Beam.select $ do
    job <- jobQ
    pull <- Beam.related_ (gitHubDb ^. #pulls) $ PullReqKey job.commit.repo $ job ^. #pullRequest
    pure (pull, job)
  case mpulljob of
    Nothing ->
      throwM
        err404
          { errBody =
              "No job for commit "
                <> LT.encodeUtf8 (LT.fromStrict $ coerce commit)
                <> " of repo "
                <> LT.encodeUtf8 (LT.fromStrict $ tshow repo)
                <> " found!"
          }
    Just (pull, job) -> pure JobDescriptor {..}
