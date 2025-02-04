{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.Webhook.Server (
  WebhookApi (..),
  webhookApp,
  GitHubSecret (..),
  WebhookEnv (..),
) where

import CMark.Utils qualified as MD
import Control.Applicative (empty)
import Control.Exception.Safe
import Control.Lens qualified as Lens
import Control.Monad (forM_, unless, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as J
import Data.ByteString.Lazy qualified as LBS
import Data.Coerce (coerce)
import Data.Foldable1 (foldMap1)
import Data.Function ((&))
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Reflection (Given)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Y
import Database.Beam (val_, (&&.), (/=.), (==.), (||.))
import Database.Beam qualified as Beam
import Database.Beam.Backend (BeamSqlBackend)
import Database.Beam.Backend.SQL.BeamExtensions qualified as Beam
import Database.Beam.Query.Internal (QNested)
import Database.Beam.Sqlite (Sqlite)
import Development.LAMMPS.GitOps.Paths (EtcDir, etcDir, getWorkerBin)
import Development.LAMMPS.GitOps.Paths.Workflow (getWorkerPidFile, getWorkerRepoConfig, repoGitOpsConfigYaml)
import Development.LAMMPS.GitOps.Types
import Development.LAMMPS.GitOps.Webhook.SlashCommand (SlashCommand (..), parseSlashCommand, toCommandParseError)
import Development.LAMMPS.GitOps.Workflow.Config (SchedulerType (..), Workflow)
import Effectful (Eff, IOE)
import Effectful.Alias
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.TimedResource
import Effectful.Database.Beam.Sqlite (SqlitePool)
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.Environment (Environment, getEnvironment, lookupEnv)
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.IO (runFileSystem)
import Effectful.FileSystem.Tagged (doesFileExist)
import Effectful.Log (Log, Logger, localDomain, logAttention, logAttention_, logInfo, logInfo_)
import Effectful.Network.GitHub.Apps (APITokens, Repository (..), callEndpointJSON, commentIssue, getRawContent, parseRawRepoAPIRequest, withGitHubRepo)
import Effectful.Network.GitHub.Apps qualified as EffGH
import Effectful.Network.Http (Http, runSimpleHttp)
import Effectful.NonDet (NonDet, OnEmptyPolicy (..), runNonDet)
import Effectful.Process.Typed
import Effectful.Process.Typed.Log (runProcessLogged_)
import Effectful.Random.Static (Random)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static.Lens qualified as EffL
import Effectful.Time (Clock, getCurrentTime)
import GHC.Generics (Generic)
import GitHub.Data (Issue, untagName)
import GitHub.Data qualified as GH
import GitHub.Data.Comments (Comment)
import GitHub.Data.Webhooks.Events (IssuesEvent, PullRequestEvent, PushEvent)
import NeatInterpolation
import Network.HTTP.Client (HttpException (HttpExceptionRequest), HttpExceptionContent (..), Response (..))
import Network.HTTP.Types (status404)
import Path.Tagged (Abs, Dir, PathTo, fromAbsDir, fromAbsFile, fromRelFile, reldir, (</>))
import Servant
import Servant.GitHub.Webhook
import Servant.Server.Generic
import System.FilePath.Posix qualified as RawFP
import System.Posix.Signals (sigTERM, signalProcess)
import Text.Show.Extra (tshow)
import Web.JWT (EncodeSigner)

newtype GitHubSecret = GitHubSecret {runGitHubSecret :: forall r. GitHubKey r}

data WebhookEnv = WebhookEnv
  { logger :: !Logger
  , apiToken :: !APITokens
  , githubConfig :: !GitHubConfig
  , scheduler :: !SchedulerType
  , githubSigner :: !EncodeSigner
  , githubDbPool :: !SqlitePool
  }
  deriving (Generic)

withGitHubDb ::
  ( Reader WebhookEnv ∈ es
  , IOE ∈ es
  ) =>
  Eff (Db.Sqlite ': es) a ->
  Eff es a
withGitHubDb act = do
  pool <- EffL.view @WebhookEnv #githubDbPool
  Db.runSqlite (Db.DbPool pool) act

instance HasContextEntry (GitHubSecret ': xs) (GitHubKey r) where
  getContextEntry (GitHubSecret x :. _) = x

data WebhookApi mode = WebhookApi
  { onIssueComment ::
      mode
        :- GitHubEvent '[ 'WebhookIssueCommentEvent]
          :> GitHubSignedReqBody '[JSON] IssueComment
          :> Post '[JSON] ()
  , onIssue ::
      mode
        :- GitHubEvent '[ 'WebhookIssuesEvent]
          :> GitHubSignedReqBody '[JSON] IssuesEvent
          :> Post '[JSON] ()
  , onPullReqeust ::
      mode
        :- GitHubEvent '[ 'WebhookPullRequestEvent]
          :> GitHubSignedReqBody '[JSON] PullRequestEvent
          :> Post '[JSON] ()
  , onPush ::
      mode
        :- GitHubEvent '[ 'WebhookPushEvent]
          :> GitHubSignedReqBody '[JSON] PushEvent
          :> Post '[JSON] ()
  }
  deriving (Generic)

data IssueComment = IssueComment
  { action :: !Text
  , issue :: !Issue
  , comment :: !Comment
  , repository :: !GH.Repo
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (J.FromJSON)

webhookApp ::
  ( Reader WebhookEnv ∈ es
  , Log ∈ es
  , IOE ∈ es
  , FileSystem ∈ es
  , Expiration ∈ es
  , Random ∈ es
  , Concurrent ∈ es
  , TypedProcess ∈ es
  , Environment ∈ es
  , Clock ∈ es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  ServerT (ToServantApi WebhookApi) (Eff es)
webhookApp =
  genericServerT
    WebhookApi
      { onIssueComment = wrapHook commentHook
      , onIssue = wrapHook issuesHook
      , onPullReqeust = wrapHook pullHook
      , onPush = wrapHook pushHook
      }

runWithGitHubRepo ::
  (Reader WebhookEnv ∈ es, Http ∈ es, Expiration ∈ es) =>
  Repository ->
  Eff (EffGH.GitHubRepo : EffGH.GitHub : es) b ->
  Eff es b
runWithGitHubRepo repo act = do
  tok <- EffL.view @WebhookEnv #apiToken
  EffGH.runGitHubWith tok $
    withGitHubRepo repo act

runWithCommentRepo ::
  ( Reader WebhookEnv ∈ es
  , Http ∈ es
  , Expiration ∈ es
  , Reader CommentContext ∈ es
  ) =>
  Eff (EffGH.GitHubRepo : EffGH.GitHub : es) b ->
  Eff es b
runWithCommentRepo act = do
  repo <- EffL.view @CommentContext #repo
  runWithGitHubRepo Repository {owner = repo.ownerName, name = repo.repoName} act

wrapHook :: (t1 -> t2) -> p -> (a, t1) -> t2
wrapHook f _ (_, a) = f a

data CommentContext = CommentContext
  { repo :: Repo
  , pull :: PullRequest
  , comment :: Comment
  , commit :: CommitHash
  }
  deriving (Generic)

commentHook ::
  ( Reader WebhookEnv ∈ es
  , Log ∈ es
  , IOE ∈ es
  , FileSystem ∈ es
  , Expiration ∈ es
  , Random ∈ es
  , Concurrent ∈ es
  , TypedProcess ∈ es
  , Environment ∈ es
  , Clock ∈ es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  IssueComment ->
  Eff es ()
commentHook evt =
  void $ runNonDet OnEmptyKeep do
    logInfo_ $ "Issue comment: " <> tshow evt
    ghcfg <- EffL.view #githubConfig
    let repoID = fromIntegral $ GH.untagId evt.repository.repoId
    when
      ( isJust evt.issue.issuePullRequest
          && evt.action
            == "created"
          && fromIntegral (GH.untagId evt.comment.commentUser.simpleUserId)
            /= ghcfg.appID
          && not ("[bot]" `T.isSuffixOf` GH.untagName evt.comment.commentUser.simpleUserLogin)
      )
      $ runSimpleHttp
      $ do
        let pullNumber = fromIntegral $ GH.unIssueNumber evt.issue.issueNumber
        rawPull <-
          runWithGitHubRepo
            Repository
              { owner = untagName evt.repository.repoOwner.simpleOwnerLogin
              , name = untagName evt.repository.repoName
              }
            $ fmap responseBody
              . callEndpointJSON @GH.PullRequest
              =<< parseRawRepoAPIRequest ("pulls/" <> show pullNumber)
        let ownerName = GH.untagName evt.repository.repoOwner.simpleOwnerLogin
            repoName = GH.untagName evt.repository.repoName
            body = evt.comment.commentBody
            comment = evt.comment
            repo :: Repo
            repo =
              Repo
                { description = fromMaybe "(N/A)" evt.repository.repoDescription
                , ..
                }
            pull :: PullRequest
            pull =
              PullRequest
                { repo = RepoKey repoID
                , pullNumber = IssueNumber pullNumber
                , branch =
                    rawPull.pullRequestHead.pullRequestCommitRef
                , title = evt.issue.issueTitle
                , author = GH.untagName evt.issue.issueUser.simpleUserLogin
                , description = fromMaybe "(N/A)" evt.issue.issueBody
                }
            commit = CommitHash rawPull.pullRequestHead.pullRequestCommitSha
        runReader CommentContext {..} $ handleAny reportErrorToPR $ do
          logInfo_ "PR comment by other user found."
          withGitHubDb $ do
            Db.transactExclusive $
              Db.liftSqliteM $
                Beam.runInsert $
                  Beam.insertOnConflict
                    gitHubDb.repos
                    (Beam.insertValues [repo])
                    Beam.anyConflict
                    Beam.onConflictUpdateAll
            Db.transactExclusive $
              Db.liftSqliteM $
                Beam.runInsert $
                  Beam.insertOnConflict
                    gitHubDb.pulls
                    (Beam.insertValues [pull])
                    Beam.anyConflict
                    Beam.onConflictUpdateAll

          repoCfgPath <- getWorkerRepoConfig repoID

          -- If no configuration, just warn and return.
          case parseSlashCommand body of
            Left err -> forM_ (toCommandParseError err) $ \msg -> do
              commentToOrigIssue $ ":warning: " <> msg
              failed $ "Parsing command failed: " <> tshow (body, err)
            Right cmd -> do
              there <- doesFileExist repoCfgPath
              unless there $ do
                logInfo_ "No repo config found"
                commentToOrigIssue $
                  MD.render
                    [ MD.para
                        [ ":warning: No configuration file found for repository "
                        , MD.code $
                            ownerName
                              <> "/"
                              <> repoName
                        , "!"
                        ]
                    , MD.para
                        [ "Please contact admin to place correct configuration file in "
                        , MD.code $ T.pack $ fromAbsFile repoCfgPath
                        ]
                    ]

              logInfo_ $ "Parsing comment: " <> tshow body
              processCommentCmd cmd

reportErrorToPR ::
  ( Reader CommentContext ∈ es
  , Http ∈ es
  , Expiration ∈ es
  , Log ∈ es
  , Reader WebhookEnv ∈ es
  ) =>
  SomeException ->
  Eff es ()
reportErrorToPR exc = do
  commentToOrigIssue $
    MD.render
      [ MD.para [":warning: Unexpected error during processing slash command!"]
      , MD.heading 2 ["Error"]
      , MD.codeBlock Nothing $ T.pack $ displayException exc
      ]

  logAttention_ $
    "Exception occurred during slash command processing: "
      <> T.pack (displayException exc)

commentToOrigIssue ::
  ( Reader CommentContext ∈ es
  , Http ∈ es
  , Expiration ∈ es
  , Reader WebhookEnv ∈ es
  ) =>
  Text ->
  Eff es ()
commentToOrigIssue msg = do
  pullNum <- EffL.view @CommentContext $ #pull . #pullNumber
  runWithCommentRepo $
    commentIssue (fromIntegral pullNum) msg

processCommentCmd ::
  ( Http ∈ es
  , Expiration ∈ es
  , Reader WebhookEnv ∈ es
  , Reader CommentContext ∈ es
  , NonDet ∈ es
  , Log ∈ es
  , IOE ∈ es
  , Random ∈ es
  , Concurrent ∈ es
  , TypedProcess ∈ es
  , Environment ∈ es
  , Clock ∈ es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  SlashCommand ->
  Eff es ()
processCommentCmd = \case
  Run mtargs -> runJob mtargs
  CancelAll -> cancelAllPRJobs
  Cancel targs -> cancelJobs targs

newtype BeamOr c b e = BeamOr {runBeamOr :: Beam.QGenExpr c b e Bool}
  deriving (Eq, Generic)
  deriving anyclass (Lens.Wrapped)

instance (BeamSqlBackend b) => Semigroup (BeamOr c b e) where
  BeamOr l <> BeamOr r = BeamOr $ l Beam.||. r

selectCancellable ::
  ( CommitF (Beam.QExpr Sqlite (QNested s)) ->
    Beam.QExpr Sqlite (QNested s) Bool
  ) ->
  Beam.Q
    Sqlite
    GitHubDb
    s
    ( CommitF (Beam.QGenExpr Beam.QValueContext Sqlite s)
    , JobF (Beam.Nullable (Beam.QGenExpr Beam.QValueContext Sqlite s))
    )
selectCancellable qry = Beam.orderBy_ (Beam.desc_ . Lens.view #createdAt . fst) do
  comm <-
    Beam.filter_ (\c -> qry c &&. Beam.not_ c.cancelled) $
      Beam.all_ gitHubDb.commits
  job <- Beam.leftJoin_ (Beam.all_ gitHubDb.jobs) $ \j ->
    j.commit ==. Beam.primaryKey comm

  Beam.guard_ $
    Beam.isNothing_ job
      ||. ( (job.status /=. val_ (Just Successed))
              &&. (job.status /=. val_ (Just Cancelled))
              &&. (job.status /=. val_ (Just Aborted))
          )
  pure (comm, job)

prettyTarg :: (Commit, Maybe Job) -> [MD.Inline]
prettyTarg (c, mj) =
  MD.text c.commit.getCommitHash
    : foldMap
      ( \j ->
          [ MD.text " ("
          , MD.code j.jobId
          , "): "
          , MD.text j.commitMessage
          ]
      )
      mj

cancelJobs ::
  ( Reader WebhookEnv ∈ es
  , Reader CommentContext ∈ es
  , Expiration ∈ es
  , Http ∈ es
  , NonDet ∈ es
  , Log ∈ es
  , IOE ∈ es
  , Random ∈ es
  , Concurrent ∈ es
  , TypedProcess ∈ es
  ) =>
  Maybe (NonEmpty CommitHash) ->
  Eff es ()
cancelJobs Nothing = localDomain "cancel-recent" $ do
  branch <- EffL.view @CommentContext $ #pull . #branch
  logInfo_ $ "Canceling most recent jobs for: " <> branch
  repo <- EffL.view @CommentContext #repo
  pull <- EffL.view @CommentContext #pull
  logInfo_ $ "Canceling most recent jobs for: " <> branch
  logInfo_ $
    "Checking if exists: (repo, pull) = "
      <> tshow (repo.repoID, pull.pullNumber)
  mtarg <-
    withGitHubDb $
      Db.notransact $
        Db.selectFirst $
          Beam.select $
            Beam.limit_ 1 $
              selectCancellable
                (\c -> (c.repo ==. RepoKey (val_ repo.repoID)) &&. (c.pull ==. val_ pull.pullNumber))
  case mtarg of
    Nothing -> do
      commentToOrigIssue [trimming|:information_source: There is no job to kill.|]
      failed "No jobs to kill"
    Just targ@(c, mj) -> do
      logAttention "Killing job" $ J.object ["commit" J..= c, "job" J..= mj]
      commentToOrigIssue $
        MD.render $
          ":information_source: Killing most recent job: "
            : prettyTarg targ
      killJob targ
cancelJobs (Just cands) = localDomain "cancel-specified" $ do
  branch <- EffL.view @CommentContext $ #pull . #branch
  logInfo_ $ "Canceling most recent jobs for: " <> branch
  repo <- EffL.view @CommentContext #repo
  logInfo_ $ "Canceling jobs for: " <> tshow (branch, cands)
  targs <-
    withGitHubDb $
      Db.notransact $
        Db.selectMany $
          Beam.select $
            selectCancellable
              ( \c ->
                  (c.repo ==. RepoKey (val_ repo.repoID))
                    &&. (foldMap1 (BeamOr . Beam.like_ c.commit . val_ . CommitHash . (<> "%") . Lens.view #getCommitHash) cands).runBeamOr
              )
  logInfo_ $ "Canceling jobs: " <> tshow (branch, targs)
  commentToOrigIssue $
    MD.render
      [ MD.para [":information_source: Following jobs will be cancelled:"]
      , MD.ul $ map (MD.para . prettyTarg) targs
      ]
  mapM_ killJob targs

cancelAllPRJobs ::
  ( Reader WebhookEnv ∈ es
  , Reader CommentContext ∈ es
  , Expiration ∈ es
  , Http ∈ es
  , NonDet ∈ es
  , Log ∈ es
  , IOE ∈ es
  , Random ∈ es
  , Concurrent ∈ es
  , TypedProcess ∈ es
  ) =>
  Eff es ()
cancelAllPRJobs = localDomain "cancel-all" $ do
  branch <- EffL.view @CommentContext $ #pull . #branch
  repo <- EffL.view @CommentContext #repo
  pull <- EffL.view @CommentContext #pull
  logInfo_ $ "Canceling all jobs for: " <> branch
  comms <-
    withGitHubDb $ Db.notransact $ Db.selectMany $ Beam.select $ do
      selectCancellable
        ( \c ->
            (c.repo ==. RepoKey (val_ repo.repoID))
              &&. (c.pull ==. val_ pull.pullNumber)
        )
  when (null comms) $ do
    commentToOrigIssue [trimming|:information_source: There is no job to kill.|]
    failed "No jobs to kill"
  logAttention "Killing jobs" comms
  commentToOrigIssue $
    MD.render
      [ ":information_source: Following job(s) will be killed:"
      , MD.ul $ map (MD.para . prettyTarg) comms
      ]
  mapM_ killJob comms

killJob ::
  ( Reader WebhookEnv ∈ es
  , Reader CommentContext ∈ es
  , Expiration ∈ es
  , Http ∈ es
  , Log ∈ es
  , IOE ∈ es
  , TypedProcess ∈ es
  , Concurrent ∈ es
  , Random ∈ es
  ) =>
  (Commit, Maybe Job) ->
  Eff es ()
killJob (c@Commit {..}, mj) = localDomain "Killer" $ do
  sched <- EffL.view @WebhookEnv #scheduler
  logInfo_ $ "Killing job " <> tshow (c, mj) <> " in scheduler"
  pidFile <- getWorkerPidFile (coerce repo) commit
  pidThere <- runFileSystem $ doesFileExist pidFile
  if pidThere
    then void $ tryAnyDeep $ do
      pid <- fmap read $ liftIO $ readFile $ fromAbsFile pidFile
      logAttention_ $ "Sending SIGTERM to " <> tshow pid
      liftIO $ signalProcess sigTERM pid
    else when (isNothing mj) do
      withGitHubDb
        $ Db.transact
        $ Db.update_
        $ Beam.updateTable
          gitHubDb.commits
          Beam.set {cancelled = Beam.toNewValue $ val_ True}
        $ \row ->
          (row.repo ==. val_ repo)
            &&. (row.commit ==. val_ commit)
      commentToOrigIssue $
        MD.render
          [ MD.para [":warning: no worker found. Sending qdel anyway.", MD.text commit.getCommitHash]
          ]
      logAttention_ "No worker found"
      forM_ mj $ \j -> do
        logInfo_ "Running job found. also sending qdel..."
        eith <- tryAny $ case sched of
          UgeDrmaa2 -> runProcessLogged_ "qdel" $ proc "qdel" [T.unpack j.jobId]
        case eith of
          Left err -> do
            commentToOrigIssue $
              MD.render
                [ MD.para [":warning: Failed to kill job ", MD.text commit.getCommitHash]
                , MD.heading 2 ["Reason"]
                , MD.codeBlock Nothing $ T.pack $ displayException err
                ]
            logAttention_ $ "Failed to kill job " <> j.jobId <> " because: " <> T.pack (displayException err)
          Right () -> logInfo_ $ "Killed: " <> tshow j
  when (isNothing mj) $
    logInfo_ "No running job for commit found."

failed :: (Log ∈ es, NonDet ∈ es) => Text -> Eff es b
failed msg = do
  logAttention_ $ "Exception: " <> msg
  empty

runJob ::
  ( Reader WebhookEnv ∈ es
  , Reader CommentContext ∈ es
  , Expiration ∈ es
  , Http ∈ es
  , NonDet ∈ es
  , Log ∈ es
  , IOE ∈ es
  , Environment ∈ es
  , Concurrent ∈ es
  , Random ∈ es
  , Clock ∈ es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  Maybe (NonEmpty CommitHash) ->
  Eff es ()
runJob Just {} = do
  commentToOrigIssue ":warning: Running specific commit(s) is unsupported"
  failed "Running specific commit(s) is unsupported"
runJob Nothing = localDomain "run-single-job" $ do
  logInfo_ "Linting YAML..."
  repo <- EffL.view @CommentContext #repo
  commit <- EffL.view @CommentContext #commit
  pull <- EffL.view @CommentContext #pull
  comment <- EffL.view @CommentContext #comment
  mjob <-
    withGitHubDb $ Db.transact $ do
      mj <- Db.selectFirst $ Beam.select $ do
        comm <-
          Beam.related_ gitHubDb.commits $
            CommitKey (val_ $ RepoKey repo.repoID) $
              val_ commit
        mj <- Beam.leftJoin_ (Beam.all_ gitHubDb.jobs) $ \j ->
          j.commit ==. Beam.primaryKey comm
        pure (comm, mj)
      when (isNothing mj) $ do
        logInfo_ "No existing commit found. inserting..."
        now <- getCurrentTime
        Db.insert_ gitHubDb.commits $
          Beam.insertValues
            [ Commit
                { repo = RepoKey repo.repoID
                , commit = commit
                , pull = pull.pullNumber
                , createdAt = now
                , cancelled = False
                }
            ]
      pure mj
  forM_ mjob $ \(comm, mJob) -> do
    commentToOrigIssue $
      MD.render $
        [ MD.para
            [ ":warning: Could not schedule a job for "
            , MD.text commit.getCommitHash
            , ": job already exists!"
            ]
        , MD.heading 2 ["Existing job"]
        , MD.para $
            "From PR #"
              : MD.text (tshow comm.pull)
              : foldMap
                ( \aJob ->
                    [ " scheduled by @"
                    , MD.text aJob.user
                    , " at "
                    , MD.text $ tshow aJob.scheduledAt
                    ]
                )
                mJob
        ]
          <> foldMap
            ( \aJob ->
                [ MD.para $
                    MD.text "Current Status: "
                      : MD.text (tshow aJob.status)
                      : foldMap
                        ( \st ->
                            " (Started: "
                              : MD.text (tshow st)
                              : foldMap
                                (\fn -> [", Finished: ", MD.text $ tshow fn])
                                aJob.finishedAt
                                <> [")"]
                        )
                        aJob.startedAt
                ]
            )
            mJob
    failed $ "Job already exists: " <> tshow (comm, mJob)
  let jsrc =
        GitOpsJobSource
          { user = GH.untagName comment.commentUser.simpleUserLogin
          , repo = repo.repoID
          , repoName = Repository {owner = repo.ownerName, name = repo.repoName}
          , pullNumber = pull.pullNumber
          , commit = commit
          }
  tok <- EffL.view @WebhookEnv #apiToken
  yamlSrc <-
    try @_ @HttpException $
      EffGH.runGitHubWith tok $
        withGitHubRepo Repository {name = repo.repoName, owner = repo.ownerName} $
          getRawContent (coerce commit) $
            fromRelFile repoGitOpsConfigYaml
  let yaml = T.pack $ fromRelFile repoGitOpsConfigYaml
      owner = repo.ownerName
      repoName = repo.repoName
      comm = commit.getCommitHash
  case Y.decodeEither' @Workflow . LBS.toStrict <$> yamlSrc of
    Right Right {} -> pure ()
    Left (HttpExceptionRequest _ (StatusCodeException r b))
      | responseStatus r == status404 -> do
          commentToOrigIssue $
            MD.render
              [ MD.para
                  [":warning: GitOps configuration not found: ", MD.code yaml]
              , MD.heading 2 ["HTTP Exception"]
              , MD.codeBlock Nothing $ T.decodeUtf8 b
              ]
          failed "Repository doesn't have gitops configuration"
    Left err -> throwM err
    Right (Left err) -> do
      commentToOrigIssue $
        MD.render
          [ MD.para
              [ ":warning: Invalid "
              , MD.link
                  [trimming|https://github.com/${owner}/${repoName}/blob/${comm}/${yaml}|]
                  [MD.code yaml]
              ]
          , MD.heading 2 ["Reason"]
          , MD.codeBlock Nothing $ T.pack $ displayException err
          ]
      failed $ "Invalid YAML: " <> tshow err
  worker <- getWorkerBin
  logInfo_ "Spawning worker..."
  let kLD_LIBRARY_PATH = "LD_LIBRARY_PATH"
      kLIBRARY_PATH = "LIBRARY_PATH"
      rootLibDir = etcDir </> [reldir|lib|]
  envs0 <- getEnvironment
  sched <- EffL.view @WebhookEnv #scheduler
  extraLibPath <-
    (<> (":" <> fromAbsDir rootLibDir)) <$> case sched of
      UgeDrmaa2 -> do
        root <- fromMaybe "/opt/uge" <$> lookupEnv "SGE_ROOT"
        pure $ root RawFP.</> "lib/lx-amd64"
  let envs =
        Map.toList
          $ Map.alter
            (Just . maybe extraLibPath (<> (":" <> extraLibPath)))
            kLD_LIBRARY_PATH
          $ Map.alter
            (Just . maybe extraLibPath (<> (":" <> extraLibPath)))
            kLIBRARY_PATH
          $ Map.fromList envs0
  logInfo "Setting env to:" envs
  ecode <-
    runTypedProcess $
      runProcess $
        proc (fromAbsFile worker) ["-d", "--root", fromAbsDir etcDir]
          & setStdin (byteStringInput $ J.encode jsrc)
          & setEnv envs
          & setNewSession True
          & setCreateGroup True
  case ecode of
    ExitSuccess -> logInfo_ "Job successfully spawned!"
    ExitFailure ec -> do
      let ec' = tshow ec
      commentToOrigIssue [trimming|:warning: Job worker for ${comm} failed to start with exit code `${ec'}`.|]
      failed $
        "Job worker for "
          <> comm
          <> " failed to start with code "
          <> tshow ec

issuesHook :: (Log ∈ es) => IssuesEvent -> Eff es ()
issuesHook issues =
  logInfo_ $ "Issues: " <> tshow issues

pullHook :: (Log ∈ es) => PullRequestEvent -> Eff es ()
pullHook pull =
  logInfo_ $ "Pull Request: " <> tshow pull

pushHook :: (Log ∈ es) => PushEvent -> Eff es ()
pushHook push =
  logInfo_ $ "Push: " <> tshow push
