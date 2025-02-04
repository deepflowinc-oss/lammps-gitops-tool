{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.Worker.Scheduling (
  withScheduledJob,
) where

import CMark.Utils qualified as MD
import Control.Lens (ReifiedFold (..), folded, (^..))
import Control.Lens qualified as Lens
import Data.Aeson qualified as J
import Data.Bifunctor qualified as Bi
import Data.Data.Lens
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Development.Job.Scheduler.UGE.DRMAA.V2 qualified as UGE
import Development.LAMMPS.GitOps.Paths.Workflow
import Development.LAMMPS.GitOps.Types
import Development.LAMMPS.GitOps.Worker.Types
import Development.LAMMPS.GitOps.Worker.Utils
import Development.LAMMPS.GitOps.Workflow.Config
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.TimedResource
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.Environment (Environment, getEnvironment)
import Effectful.FileSystem.Tagged
import Effectful.JobScheduler (getJobInfo, runSchedulerIO, scheduleJob)
import Effectful.JobScheduler qualified as JS
import Effectful.Log.Extra
import Effectful.Process.Typed
import Effectful.Random.Static (Random)
import Effectful.Reader.Static
import Effectful.Reader.Static.Lens qualified as EffL
import Language.Bash.Parse qualified as Bash
import Language.Bash.Pretty qualified as Bash
import Language.Bash.Syntax qualified as Bash
import Language.Bash.Word qualified as Bash
import Path.Tagged (fromAbsDir, fromAbsFile, (</>))
import Path.Tagged.IO ()
import System.Exit (exitFailure)
import Text.Show.Extra (tshow)

withScheduledJob ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , IOE :> es
  , FileSystem :> es
  , Expiration :> es
  , Random :> es
  , Concurrent :> es
  , Environment :> es
  , TypedProcess :> es
  ) =>
  (JS.Job -> Eff (JS.Scheduler ': es) a) ->
  Eff es a
withScheduledJob k = do
  sched <- EffL.view $ #workerConfig . #scheduler
  let runner = case sched of
        UgeDrmaa2 -> \act -> do
          baseName <- jobBaseName
          let name = baseName <> "-js"
          runSchedulerIO (UGE.defaultConfig name) act
  runner $ do
    jobSpec <- buildJobSpec
    logInfo "Scheduling job" jobSpec
    job <- scheduleJob jobSpec
    logInfo_ "Job scheduled successfully!"
    comm <- EffL.view #commit
    repoID <- EffL.view #repo
    pullNumber <- EffL.view #pullNumber
    logTrace_ "Obtaining job info..."
    jinfo <- getJobInfo job
    logTrace "Initial job info" jinfo
    user <- EffL.view #jobUser
    commitMessage <- EffL.view #commitMessage
    logInfo_ "Inserting new job to Database..."
    withGitHubDb
      $ Db.transactExclusive
      $ Db.insert_
        gitHubDb.jobs
      $ Db.insertValues
        [ Job
            { user
            , commitMessage
            , status = convertJobStatus jinfo.jobStatus
            , scheduledAt = jinfo.submittedAt
            , pullRequest = pullNumber
            , commit = CommitKey (RepoKey repoID) comm
            , startedAt = jinfo.startedAt
            , finishedAt = jinfo.finishedAt
            , jobName = jinfo.jobName
            , jobId = jinfo.jobID
            }
        ]
    logInfo_ "Done! Switching to main job loop..."
    k job

getEnvVars ::
  ( TypedProcess :> es
  , Log :> es
  , IOE :> es
  , Reader WorkerEnv :> es
  , Expiration :> es
  ) =>
  [Text] ->
  Eff es (HashMap Text Text)
getEnvVars modules
  | null modules = pure mempty
  | otherwise = do
      (out, err0) <- readProcess_ $ proc "modulecmd" $ "bash" : "load" : map T.unpack modules
      case Bash.parse "modulecmd" $ LT.unpack $ LT.decodeUtf8 out of
        Right (Bash.List stmts) -> do
          let vars =
                stmts
                  ^.. folded
                    . biplate @_ @Bash.Assign
                    . #_Assign
                    . runFold do
                      k <- Fold Lens._1
                      v <- Fold $ Lens._3 . #_RValue
                      pure (T.pack $ Bash.prettyText k, T.pack $ Bash.unquote v)
          pure $ HM.fromList vars
        Left err -> do
          let sout = LT.decodeUtf8 out
              serr = LT.decodeUtf8 err0
          commentToPR $
            MD.render $
              [ ":warning: Could not schedule job: module loading failed!"
              , MD.para ["modules:"]
              , MD.ul [MD.para [MD.text m] | m <- modules]
              , MD.heading 2 ["Parse Error"]
              , MD.codeBlock Nothing $ tshow err
              , MD.heading 2 ["modulecmd outputs"]
              ]
                <> MD.details "Stdout" (LT.toStrict sout)
                <> MD.details "Stderr" (LT.toStrict serr)
          logAttention "modulecmd returns invalid bash script" $
            J.object
              [ "modules" J..= modules
              , "parse error" J..= show err
              , "stdout" J..= sout
              , "stderr" J..= serr
              ]
          liftIO exitFailure

buildJobSpec ::
  ( Reader WorkerEnv :> es
  , IOE :> es
  , FileSystem :> es
  , Environment :> es
  , TypedProcess :> es
  , Log :> es
  , Expiration :> es
  ) =>
  Eff es JS.JobSpec
buildJobSpec = do
  jobName <- jobBaseName
  wf <- EffL.view #workflow
  cloned <- viewClonedDir
  let workdir = fromAbsDir cloned
  repoID <- EffL.view #repo
  comm <- EffL.view #commit
  logsDir <- getJobLogsDir repoID comm
  createDirIfMissing True logsDir
  (command, args) <- case wf.job of
    Script (Exec src) -> pure ("bash", ["-c", src])
    Script (Path fp) -> (,[]) . fromAbsDir <$> resolveDir cloned fp
    DVCRepro margs -> cmdArgsWithToolchain "dvc" $ "repro" : maybe [] NE.toList margs
  let duration = wf.resources.duration
      memory = Nothing
      queue = wf.queue
      numNodes = wf.resources.nodes
      gpus = wf.resources.gpus
      coresPerNode = Just wf.resources.coresPerNode
      priority = wf.priority
      stdoutLog = Just $ fromAbsFile $ logsDir </> jobStdoutLogFile
      stderrLog = Just $ fromAbsFile $ logsDir </> jobStderrLogFile
      joinLogs = Just False
      startTime = Nothing
  vars <- getEnvVars $ fromMaybe [] wf.modules
  let modifyOMP
        | wf.resources.coresPerNode > 0 =
            HM.insert
              "OMP_NUM_THREADS"
              $ tshow wf.resources.coresPerNode
        | otherwise = id
      withModuleVars
        | HM.null vars = id
        | otherwise = HM.union vars

  environment <-
    modifyOMP
      . withModuleVars
      . HM.fromList
      . map (Bi.bimap T.pack T.pack)
      <$> getEnvironment
  pure JS.JobSpec {..}
