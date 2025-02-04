{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.Worker (
  defaultMain,
  versionInfo,
  runWorker,
  mainLoop,
) where

import CMark.Utils qualified as MD
import Control.Concurrent qualified as Conc
import Control.Exception.Safe (Exception (..), SomeException (..), bracket, bracket_, finally, handleAny, throwM, tryAny, uninterruptibleMask_)
import Control.Lens qualified as Lens
import Control.Monad (forM_, unless, when)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Bifunctor qualified as Bi
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Function ((&))
import Data.Functor (void)
import Data.Maybe (fromJust, fromMaybe)
import Data.Reflection (Given, give)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as T
import Data.Yaml qualified as Y
import Database.Beam (toNewValue, (&&.), (==.))
import Database.Beam qualified as Beam
import Database.Beam.Query (val_)
import Development.LAMMPS.GitOps.Options qualified as AppOpt
import Development.LAMMPS.GitOps.Paths (EtcDir, defaultEtcDir, getDaemonConfigYaml, getGitHubAppKeyFile, getGitHubDbFile, repoGitOpsConfigYaml)
import Development.LAMMPS.GitOps.Paths.Workflow (getJobCloneDir, getJobLogsDir, getSingleJobDir, getWorkerPidFile, getWorkerRepoConfig, jobWorkerLogFile)
import Development.LAMMPS.GitOps.Types
import Development.LAMMPS.GitOps.Worker.DVC (finaliseDvcRepro, isDvcEnabled, setupDvc)
import Development.LAMMPS.GitOps.Worker.Report (updateReportIndex, uploadReport)
import Development.LAMMPS.GitOps.Worker.Scheduling
import Development.LAMMPS.GitOps.Worker.Toolchain
import Development.LAMMPS.GitOps.Worker.Types
import Development.LAMMPS.GitOps.Worker.Utils (commentToPR, commentToPR', syncJobInfoToDb)
import Development.LAMMPS.GitOps.Workflow.Config (Toolchain (..))
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent, threadDelay)
import Effectful.Concurrent.Async (race, race_)
import Effectful.Concurrent.STM (atomically, newEmptyTMVarIO, readTMVar, tryPutTMVar)
import Effectful.Concurrent.TimedResource
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.Dispatch.Static (unsafeConcUnliftIO)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.Exception (throwString)
import Effectful.FileSystem.Tagged
import Effectful.JobScheduler (waitStarted, waitTerminated)
import Effectful.JobScheduler qualified as JS
import Effectful.Log.Extra
import Effectful.Network.GitHub.Apps hiding (CommitHash)
import Effectful.Network.GitHub.Apps qualified as Apps
import Effectful.Network.Http (Response (..), runSimpleHttp)
import Effectful.Process.Typed (TypedProcess, proc, runTypedProcess, setWorkingDir)
import Effectful.Process.Typed.Log (runProcessLogged_)
import Effectful.Random.Static
import Effectful.Reader.Static
import Effectful.Reader.Static.Lens qualified as EffL
import Effectful.Time (Clock, runClock)
import GHC.Generics (Generic)
import GitHub.REST.Auth (Token (..), loadSigner)
import NeatInterpolation (trimming)
import Options.Applicative qualified as Opt
import Path.Tagged (Abs, Dir, PathTo, fromAbsDir, fromAbsFile, parent, parseAbsDir, relfile, untagPath, (</>))
import Path.Tagged.IO qualified as PT
import Paths_lammps_gitops_worker (version)
import System.Exit (ExitCode (..), exitFailure)
import System.Posix (createSession, exitImmediately, getProcessID, installHandler, sigTERM)
import System.Posix.IO qualified as Pos
import System.Posix.Process (forkProcess)
import System.Posix.Signals qualified as Pos
import Text.Show.Extra (tshow)

versionInfo :: AppOpt.VersionInfoQ
versionInfo = AppOpt.versionInfo "LAMMPS GitOps Worker" version

data WorkerOpts = WorkerOpts
  { daemon :: !Bool
  , rootDir :: !(PathTo EtcDir Abs Dir)
  }
  deriving (Show, Eq, Ord, Generic)

workerAppP :: Opt.ParserInfo WorkerOpts
workerAppP =
  Opt.info p $
    Opt.progDesc "GitOps Tool Worker, worker for interacting with jobs."
  where
    p = do
      daemon <-
        Opt.switch $
          Opt.long "daemon"
            <> Opt.short 'd'
            <> Opt.help "Fork process as a daemon"
      rootDir <-
        Opt.option (Opt.eitherReader $ Bi.first displayException . parseAbsDir) $
          Opt.long "root"
            <> Opt.short 'R'
            <> Opt.value defaultEtcDir
            <> Opt.showDefault
            <> Opt.help "root directory"
      pure WorkerOpts {..}

defaultMain :: (MonadIO m) => AppOpt.VersionInfo -> m ()
defaultMain vinfo = liftIO $ do
  WorkerOpts {..} <- AppOpt.execOptionParserWithVersion vinfo workerAppP
  jsrc <- either throwString pure . J.eitherDecode =<< LBS.getContents
  give rootDir $
    runWorker jsrc (mainLoop vinfo)
      & if daemon then daemonise else id

daemonise :: IO () -> IO ()
daemonise act =
  bracket Conc.getNumCapabilities Conc.setNumCapabilities $ \numCap -> do
    Conc.setNumCapabilities 1
    forkProcess $ do
      void createSession
      void $ forkProcess $ do
        bracket (Pos.openFd "/dev/null" Pos.ReadOnly Nothing Pos.defaultFileFlags) Pos.closeFd $ \devnull ->
          mapM_ (Pos.dupTo devnull) [Pos.stdInput, Pos.stdOutput, Pos.stdError]
        Conc.setNumCapabilities numCap
        act
    exitImmediately ExitSuccess

installSigTermHandler :: IO () -> IO Pos.Handler
installSigTermHandler act =
  installHandler sigTERM (Pos.CatchOnce act) Nothing

runWorker ::
  ( MonadIO m
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  GitOpsJobSource ->
  Eff
    '[ Reader WorkerEnv
     , Expiration
     , Environment
     , TypedProcess
     , Clock
     , Random
     , Concurrent
     , FileSystem
     , Log
     , IOE
     ]
    a ->
  m a
runWorker jsrc@GitOpsJobSource {..} act = liftIO $ do
  gen <- getStdGen
  logD <- getJobLogsDir repo commit
  PT.createDirIfMissing True logD
  pid <- getProcessID
  pidFile <- getWorkerPidFile repo commit
  githubDbPath <- getGitHubDbFile
  bracket_
    (T.writeFile (fromAbsFile pidFile) $ tshow pid)
    (tryAny $ PT.removeFile pidFile)
    $ runEff
    $ runFileLogger "worker" (logD </> jobWorkerLogFile) LogTrace
    $ runFileSystem
    $ runConcurrent
    $ evalRandom gen
    $ runClock
    $ runTypedProcess
    $ runEnvironment
    $ Db.withSqlPool (untagPath githubDbPath)
    $ \githubPool ->
      runExpiration do
        signer <- liftIO . loadSigner . fromAbsFile =<< getGitHubAppKeyFile
        workerCfgPath <- getDaemonConfigYaml
        cfg@WorkerConfig {..} <- liftIO $ Y.decodeFileThrow $ fromAbsFile workerCfgPath
        let apiTokenConfig =
              APITokenConfig
                { repos = github.repos
                , github =
                    Apps.GitHubConfig
                      { privKey = signer
                      , appName = fromMaybe "LAMMPS GitOps Tool" github.appName
                      , appID = github.appID
                      }
                }
        wenv <- setupWorkerEnv githubPool apiTokenConfig cfg jsrc
        runReader wenv do
          handleAny (reportErrorToPR pullNumber commit) $ do
            cloned <- getJobCloneDir repo commit
            -- NOTE: installHandler overrides existing one.
            -- Setup properly
            withEffToIO $ \runInIO -> void $ installSigTermHandler $ do
              runInIO $ logInfo_ "SIGTERM received during initialisation. Arbotring..."
              runInIO $ recordCanceled githubPool repo commit
              void $ tryAny $ PT.removePathForcibly pidFile
              thereClone <- PT.doesDirExist cloned
              when thereClone $ do
                void $ tryAny $ PT.removePathForcibly pidFile
              exitImmediately (ExitFailure 1)
            createDirIfMissing True $ parent pidFile
            let commStr = commit.getCommitHash
            commentToPR'
              pullNumber
              [trimming|:information_source: Preparing job for ${commStr}... (This may take a while)|]
            a <- do
              void $ unsafeConcUnliftIO Persistent Unlimited $ \runInIO ->
                flip (installHandler sigTERM) Nothing $ Pos.Catch $ runInIO do
                  commentToPR [trimming|:warning: cancellation of ${commStr} has been taken place...|]
                  logAttention_ "Signal handler detects SIGTERM (cancel)!"
                  recordCanceled githubPool repo commit
                  void $ atomically $ tryPutTMVar wenv.cancelSwitch ()
              act
            logInfo_ "Everything is done!"
            pure a

recordCanceled ::
  (IOE :> es, Concurrent :> es, Log :> es, Random :> es) =>
  Db.SqlitePool ->
  RepoID ->
  CommitHash ->
  Eff es ()
recordCanceled pool repo commit = do
  Db.runSqlite (Db.DbPool pool)
    $ Db.transact
    $ Db.update_
    $ Beam.updateTable
      gitHubDb.commits
      Beam.set {cancelled = toNewValue $ val_ True}
    $ \row ->
      (row.repo ==. RepoKey (val_ repo))
        &&. (row.commit ==. val_ commit)

reportErrorToPR ::
  ( IOE :> es
  , Expiration :> es
  , Log :> es
  , Reader WorkerEnv :> es
  ) =>
  IssueNumber ->
  CommitHash ->
  SomeException ->
  Eff es a
reportErrorToPR _ _ e
  | Just {} <- fromException @ExitCode e = throwM e
reportErrorToPR pr (CommitHash commit) (SomeException exc) = do
  logTrace_ $ "Reporting to the PR " <> tshow (pr, commit, exc)
  commentToPR'
    pr
    $ MD.render
      [ MD.para
          [ ":warning: Experiment for "
          , MD.text commit
          , " aborted with uneepxected exception!"
          ]
      , MD.heading 2 ["Exception"]
      , MD.codeBlock Nothing $ T.pack $ displayException exc
      ]
  throwM exc

newtype CommitMetaData = CommitMetaData {commit :: CommitPayload}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

newtype CommitPayload = CommitPayload {message :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

setupWorkerEnv ::
  ( Log :> es
  , IOE :> es
  , TypedProcess :> es
  , Expiration :> es
  , Random :> es
  , Concurrent :> es
  , FileSystem :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  Db.SqlitePool ->
  APITokenConfig ->
  WorkerConfig ->
  GitOpsJobSource ->
  Eff es WorkerEnv
setupWorkerEnv githubDbPool apiTokenCfg workerConfig GitOpsJobSource {..} = do
  apiToken <- newAPITokens apiTokenCfg
  let commStr = T.unpack commit.getCommitHash
  commMeta <-
    runSimpleHttp $ runGitHubWith apiToken $ withGitHubRepo repoName $ do
      fmap responseBody
        . callEndpointJSON @CommitMetaData
        =<< parseRawRepoAPIRequest ("commits/" <> T.unpack commit.getCommitHash)
  (repoF, branch) <-
    Db.runSqlite (Db.DbPool githubDbPool) $
      Db.notransact $
        do
          bra <-
            fmap (Lens.view #branch . fromJust) $
              Db.selectFirst $
                Beam.lookup_ gitHubDb.pulls $
                  PullReqKey (RepoKey repo) pullNumber
          repoF <-
            fmap fromJust $
              Db.selectFirst $
                Beam.lookup_ gitHubDb.repos $
                  RepoKey repo
          pure (repoF, bra)
  workerRepoCfgPath <- getWorkerRepoConfig repo
  repoCfgThere <- doesFileExist workerRepoCfgPath
  unless repoCfgThere $
    throwString $
      "No configuration file found for repository `"
        <> T.unpack repoF.ownerName
        <> "/"
        <> T.unpack repoF.repoName
        <> "`; "
        <> " please contact admin to place correct configuration file in `"
        <> fromAbsFile workerRepoCfgPath
        <> "`"
  repoConfig <-
    either
      ( \err ->
          throwString $
            T.unpack $
              MD.render
                [ MD.para
                    [ "Sytnax error in repository config file: "
                    , MD.code $ T.pack $ fromAbsFile workerRepoCfgPath
                    , "; please contact admin!"
                    ]
                , MD.heading 2 ["Reason"]
                , MD.codeBlock Nothing $ T.pack $ displayException err
                ]
      )
      pure
      =<< liftIO (Y.decodeFileEither $ fromAbsFile workerRepoCfgPath)

  clonedDir <- getJobCloneDir repo commit
  let commitMessage = commMeta.commit.message
      jobUser = user
  tok <-
    askRepoToken repoName apiToken
      >>= \case
        Just (AccessToken tok) -> pure $ TE.decodeUtf8 tok
        Just (BearerToken tok) -> pure $ TE.decodeUtf8 tok
        Nothing -> throwString $ "No Repository token found for: " <> show repoName
  let cloneUrl = "https://x-access-token:" <> tok <> "@github.com/" <> repoF.ownerName <> "/" <> repoF.repoName
  localDomain "clone" $ do
    logInfo_ $ "Checking out directory to: " <> T.pack (fromAbsDir clonedDir)
    createDirIfMissing True $ parent clonedDir
    alreadyCloned <- doesDirExist clonedDir
    when alreadyCloned $
      removeDirRecur clonedDir
    runProcessLogged_ "git-clone" $
      proc
        "git"
        [ "clone"
        , T.unpack cloneUrl
        , fromAbsDir clonedDir
        , "--depth"
        , "1"
        , "--branch"
        , T.unpack branch
        ]

    runProcessLogged_ "fetch" $
      proc "git" ["fetch", "origin", "--depth", "1", commStr]
        & setWorkingDir (fromAbsDir clonedDir)
    runProcessLogged_ "checkout" $
      proc "git" ["switch", "--detach", commStr]
        & setWorkingDir (fromAbsDir clonedDir)
    logInfo_ "Successfully checked out."
  toolchain <- fromMaybe Global <$> detectToolchain clonedDir
  workflow <-
    liftIO $
      Y.decodeFileThrow $
        fromAbsFile $
          clonedDir
            </> repoGitOpsConfigYaml
  cancelSwitch <- newEmptyTMVarIO
  pure
    WorkerEnv
      { repoName =
          Repository
            { owner = repoF.ownerName
            , name = repoF.repoName
            }
      , ..
      }

mainLoop ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , IOE :> es
  , FileSystem :> es
  , TypedProcess :> es
  , Expiration :> es
  , Random :> es
  , Concurrent :> es
  , Environment :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  AppOpt.VersionInfo ->
  Eff es ()
mainLoop vinfo = localDomain "mainLoop" $ do
  comm <- EffL.view #commit
  let commTxt = comm.getCommitHash

  bracket setupEnvs cleanupEnvs $ \setup -> forM_ setup $ const $ do
    localDomain "schedule" $ do
      logInfo_ $ T.pack $ AppOpt.formatVersionInfo vinfo
      logInfo_ "Scheduling job..."
      withScheduledJob $ \j -> do
        -- NOTE: For some reason, monitorTargetJob thread cannot be killed
        -- when waitForCancel.
        (monitorTargetJob j `race_` waitForCancel j) `finally` localDomain "finalise-job" do
          logTrace_ "Finalising job..."
          initInfo <- JS.getJobInfo j
          logTrace_ "Job info attained."
          reportUri <- runSimpleHttp uploadReport
          logTrace_ "Report Uploaded"
          commentToPR $
            MD.render
              [ ":chart_with_upwards_trend: The experiment report for "
              , MD.text commTxt
              , " ("
              , MD.code initInfo.jobName
              , ", "
              , MD.code initInfo.jobID
              , ")"
              , " has been uploaded to "
              , MD.link (tshow reportUri) [MD.code $ tshow reportUri]
              ]
          logTrace_ "Report commented to PR"
          runSimpleHttp updateReportIndex
          logTrace_ "Report Index Updated."
    logInfo_ "Experiment success!"
    repo <- EffL.view #repo
    j <-
      fmap fromJust $
        withGitHubDb $
          Db.notransact $
            Db.selectFirst $
              Beam.lookup_ gitHubDb.jobs $
                JobKey $
                  CommitKey (RepoKey repo) comm
    commentToPR $
      MD.render
        [ ":white_check_mark: The experiment "
        , MD.text commTxt
        , " ("
        , MD.code j.jobName
        , ", "
        , MD.code j.jobId
        , ")"
        , " done successfully!"
        ]

  -- NOTE: We cleanup cloned directory ONLY WHEN the entire computation is successful.
  -- So we MUST cleanup cloned repository here, outside cleanup.
  uninterruptibleMask_ $ do
    logInfo_ "Cleaning up cloned directory as the experiment successed..."
    dir <- viewClonedDir
    void $ tryAny $ removeDirRecur dir
  logInfo_ "Everything is done!"

waitForCancel ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , Concurrent :> es
  , JS.Scheduler :> es
  , IOE :> es
  , Expiration :> es
  , Random :> es
  ) =>
  JS.Job ->
  Eff es ()
waitForCancel j = localDomain "cancel-detection" do
  atomically . readTMVar =<< EffL.view #cancelSwitch
  logAttention_ "Cancel switch is pushed!"
  repoID <- EffL.view #repo
  comm <- EffL.view #commit
  let commStr = comm.getCommitHash
  uninterruptibleMask_ $ do
    logTrace_ "Cancelling..."
    JS.cancel j
    logTrace_ "cancel requested to job scheduler."
    jinfo <- withGitHubDb $ Db.transact do
      Db.update_
        $ Beam.updateTable
          gitHubDb.jobs
          Beam.set {status = toNewValue $ val_ Cancelled}
        $ \row ->
          (row.commit.repo ==. RepoKey (val_ repoID))
            &&. (row.commit.commit ==. val_ comm)
      logTrace_ "Updated Jobs DB"
      fmap fromJust $
        Db.selectFirst $
          Beam.lookup_ gitHubDb.jobs $
            JobKey $
              CommitKey (RepoKey repoID) comm
    logTrace "Updated job info" $ J.object ["jinfo" J..= jinfo]
    let jobID = jinfo.jobId
    commentToPR
      [trimming|:warning: The experiment ${commStr} (`${jobID}`) was cancelled.|]
    logTrace_ "Commented to PR. Exiting..."
    threadDelay (10 * 10 ^ (6 :: Int))
    -- Make sure the worker exits successfully on early cancel.
    -- NOTE: We MUSTNOT @exitSuccess@ here. It just throws ExitCode exception,
    -- but racing thread can be blocked indefinitely on FFI call, which prevents
    -- worker to exit immediately. We MUST use 'exitImmediately', which uses
    -- syscall to exit, to ensure immediate exit.
    liftIO $ exitImmediately ExitSuccess

monitorTargetJob ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , IOE :> es
  , Expiration :> es
  , Random :> es
  , Concurrent :> es
  , JS.Scheduler :> es
  ) =>
  JS.Job ->
  Eff es ()
monitorTargetJob job = do
  logInfo_ "Monitoring job..."
  loop `finally` saveFinalInfo
  where
    saveFinalInfo =
      localDomain "cleanup" $ syncJobInfoToDb job
    loop = localDomain "monitor" $ do
      logInfo_ "Obtaining first monitoring info..."
      jinfo <- JS.getJobInfo job
      logInfo "Obtained." jinfo
      comm <- EffL.view #commit
      repoID <- EffL.view #repo
      dest <- getSingleJobDir repoID comm
      logInfo_ "Reporting to the original PR..."
      commentToPR $
        MD.render
          [ MD.para
              [ ":calendar: An experiment for "
              , MD.text comm.getCommitHash
              , " has been scheduled as "
              , MD.code jinfo.jobID
              ]
          , MD.para
              [ "Outputs will be written to "
              , MD.code $ T.pack $ fromAbsDir dest
              ]
          ]
      logInfo_ "Waiting for job to start..."
      st0 <-
        maybe
          (throwString "Impossible: No satus available after waitStart")
          pure
          =<< waitStarted Nothing job
      logInfo "Job status after waitSterted" st0
      syncJobInfoToDb job
      finSt <- case st0 of
        JS.Success -> pure st0
        JS.Running -> do
          logInfo_ "Job started. Waiting for termination..."
          commentToPR $
            MD.render
              [ ":stopwatch: Job started: "
              , MD.text comm.getCommitHash
              , " ("
              , MD.code jinfo.jobName
              , ", "
              , MD.code jinfo.jobID
              , ")"
              ]
          maybe
            (throwString "Impossible: No satus available after waitStart")
            pure
            =<< waitTerminated Nothing job
        JS.Cancelled r -> do
          commentToPR $
            MD.render $
              MD.para
                [ ":warning: Experiment for "
                , MD.text comm.getCommitHash
                , " ("
                , MD.code jinfo.jobName
                , ", "
                , MD.code jinfo.jobID
                , ") was cancelled!"
                ]
                : maybe
                  []
                  ( \s ->
                      [ MD.heading 2 ["Message"]
                      , MD.blockQuote [MD.para [MD.text $ T.pack s]]
                      ]
                  )
                  r
          logAttention_ $ "Job cancelled: " <> tshow r
          liftIO exitFailure
        JS.Queued -> reportAbortionAndQuit $ Just $ "Invalid status after waitStarted: " <> show st0
        JS.Held -> reportAbortionAndQuit $ Just $ "Invalid status after waitStarted: " <> show st0
        JS.Suspended -> reportAbortionAndQuit $ Just $ "Invalid status after waitStarted: " <> show st0
        JS.Aborted -> reportAbortionAndQuit Nothing
        JS.Errored msg -> reportAbortionAndQuit $ Just msg
      logInfo "waitTerminate result" finSt
      logInfo_ "Syncing final jobinfo to Db..."
      syncJobInfoToDb job
      logInfo_ "Done."
      case finSt of
        JS.Success -> logInfo_ "Job successed!"
        JS.Errored str -> reportAbortionAndQuit $ Just str
        JS.Aborted -> reportAbortionAndQuit Nothing
        st ->
          reportAbortionAndQuit $
            Just $
              "Job resulted in unexpected state after waitTerminated: "
                <> show st

newtype SetupInfo = SetupInfo {isDvc :: Bool}
  deriving (Show, Eq, Ord, Generic)

setupEnvs ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , FileSystem :> es
  , TypedProcess :> es
  , Environment :> es
  , Concurrent :> es
  ) =>
  Eff es (Maybe SetupInfo)
setupEnvs = localDomain "Setup" $ handleCancel $ do
  cloned <- viewClonedDir
  tc <- EffL.view #toolchain
  initToolchain tc cloned
  isDvc <- isDvcEnabled
  when isDvc $ do
    logInfo_ "DVC detected. initialising..."
    setupDvc
  pure SetupInfo {..}

handleCancel ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , Concurrent :> es
  ) =>
  Eff es a ->
  Eff es (Maybe a)
handleCancel act = do
  either id id
    <$> (Just <$> act)
      `race` ( Nothing <$ do
                switch <- EffL.view #cancelSwitch
                atomically $ readTMVar switch
                logAttention_ "Cancellation is detected!"
             )

cleanupEnvs ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , IOE :> es
  , FileSystem :> es
  , TypedProcess :> es
  , Expiration :> es
  , Random :> es
  , Concurrent :> es
  ) =>
  Maybe SetupInfo ->
  Eff es ()
cleanupEnvs Nothing = localDomain "cleanup(cancel)" $ do
  logInfo_ "Cancellation detected! Nuke all cloned files."
  clone <- viewClonedDir
  void $ tryAny $ removeDirRecur clone
  logInfo_ "Removed entire cloned repo."
cleanupEnvs (Just SetupInfo {..}) = localDomain "cleanup" $ do
  when isDvc finaliseDvcRepro
  clone <- viewClonedDir
  let dvcConfigLocal = clone </> [relfile|.dvc/config.local|]
  localConfThere <- doesFileExist dvcConfigLocal
  when localConfThere $ removeFile dvcConfigLocal
  logInfo_ "Cleaned-up cloned repo."

reportAbortionAndQuit ::
  ( IOE :> es
  , Log :> es
  , Reader WorkerEnv :> es
  , Expiration :> es
  ) =>
  Maybe String ->
  Eff es a
reportAbortionAndQuit mmsg = do
  comm <- EffL.view #commit
  logAttention_ $ "Job aborted! Reason: " <> tshow mmsg
  void $
    tryAny $
      commentToPR $
        MD.render $
          MD.para [":warning: Some experiments for ", MD.text comm.getCommitHash, " aborted abnormally!"]
            : maybe
              []
              ( \msg ->
                  [ MD.heading 2 ["Reason"]
                  , MD.codeBlock Nothing $ T.pack msg
                  ]
              )
              mmsg

  liftIO exitFailure
