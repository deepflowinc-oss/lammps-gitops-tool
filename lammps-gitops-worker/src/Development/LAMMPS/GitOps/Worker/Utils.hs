{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.Worker.Utils (
  commentToPR,
  commentToPR',
  jobBaseName,
  cmdArgsWithToolchain,
  updateStatus,
  syncJobInfoToDb,
) where

import Control.Applicative (optional, (<|>))
import Data.Char (isAlphaNum, isAscii)
import Data.Functor (void)
import Data.Text (Text)
import Data.Text qualified as T
import Database.Beam (val_, (<-.), (==.))
import Database.Beam qualified as Beam
import Development.LAMMPS.GitOps.Types
import Development.LAMMPS.GitOps.Worker.Types
import Development.LAMMPS.GitOps.Workflow.Config (Toolchain (..))
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.TimedResource
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.JobScheduler qualified as JS
import Effectful.Log.Extra
import Effectful.Network.GitHub.Apps
import Effectful.Network.Http (runSimpleHttp)
import Effectful.Random.Static (Random)
import Effectful.Reader.Static
import Effectful.Reader.Static.Lens qualified as EffL
import NeatInterpolation (trimming)
import Text.Regex.Applicative.Text qualified as RE
import Text.Show.Extra (tshow)

commentToPR ::
  ( Reader WorkerEnv :> es
  , IOE :> es
  , Expiration :> es
  ) =>
  Text ->
  Eff es ()
commentToPR text = do
  !prNum <- EffL.view #pullNumber
  commentToPR' prNum text

commentToPR' ::
  ( IOE :> es
  , Expiration :> es
  , Reader WorkerEnv :> es
  ) =>
  IssueNumber ->
  Text ->
  Eff es ()
commentToPR' prNum text = do
  runSimpleHttp $
    runRepoAPI $
      commentIssue (fromIntegral prNum) (reductAccessToken text)

reductAccessToken :: Text -> Text
reductAccessToken = RE.replace ("<REDUCTED>" <$ secretsP)
  where
    secretsP =
      void $
        optional (RE.string "x-access-token:")
          *> ( void (RE.string "gh" *> RE.psym (`elem` ("pousr" :: String)))
                <|> void (RE.string "github_pat")
             )
          *> RE.sym '_'
          *> RE.some (RE.psym $ \c -> isAscii c && isAlphaNum c || c == '_')

jobBaseName ::
  (Reader WorkerEnv :> es) =>
  Eff es Text
jobBaseName = do
  repo <- tshow <$> EffL.view #repo
  comm <- T.take 7 <$> EffL.view (#commit . #getCommitHash)
  pure [trimming|gitops-${repo}-${comm}|]

cmdArgsWithToolchain :: (Reader WorkerEnv :> es) => FilePath -> [Text] -> Eff es (FilePath, [Text])
cmdArgsWithToolchain cmd args =
  EffL.view #toolchain >>= \case
    Global -> pure (cmd, args)
    Rye -> pure ("rye", "run" : "--" : T.pack cmd : args)
    Poetry -> pure ("poetry", "run" : "--" : T.pack cmd : args)

updateStatus ::
  ( Reader WorkerEnv :> es
  , Concurrent :> es
  , Random :> es
  , IOE :> es
  , Log :> es
  ) =>
  JobStatus ->
  Eff es ()
updateStatus st = do
  repoID <- EffL.view #repo
  comm <- EffL.view #commit
  withGitHubDb $
    Db.transactExclusive $
      Db.update_ $
        Beam.update
          gitHubDb.jobs
          (\j -> j.status <-. val_ st)
          (\j -> j.commit ==. val_ (CommitKey (RepoKey repoID) comm))

syncJobInfoToDb ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , IOE :> es
  , Random :> es
  , Concurrent :> es
  , JS.Scheduler :> es
  ) =>
  JS.Job ->
  Eff es ()
syncJobInfoToDb job = localDomain "sync-jinfo" $ do
  logInfo_ "Syncing recent jobinfo to Db. querying..."
  repoID <- EffL.view #repo
  comm <- EffL.view #commit
  jinfo <- JS.getJobInfo job
  logInfo "Query retrieved" jinfo
  withGitHubDb $ Db.transact $ Db.update_ $ toJobUpdate repoID comm jinfo
  logInfo_ "Job info saved."
