{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.App.Client (
  defaultMain,
  versionInfo,
  defaultMainWith,

  -- * Options
  CliOpts (..),
  APIConfig (..),
  cliOptsP,
  CliCmd (..),
  JobTarget (..),
  jobTargetP,
  scheduleP,
  cancelP,
  statusP,
  StatusOpts (..),
  DeployOpts (..),
  deployP,
  EnvVars (..),
  getEnvVars,
) where

import Barbies
import Brick (BrickEvent (..), hScrollBy, vScrollBy, vScrollPage, vScrollToBeginning, vScrollToEnd, viewportScroll)
import Brick qualified
import Brick.Widgets.Table qualified as Brick
import Control.Applicative
import Control.Exception.Safe
import Control.Lens
import Control.Monad (forM_)
import Control.Monad.Morph (hoist)
import Data.Aeson (FromJSON, ToJSON (..))
import Data.Aeson qualified as A
import Data.ByteString.Lazy.Char8 qualified as LBS
import Data.Char qualified as C
import Data.Coerce (coerce)
import Data.Functor (void)
import Data.Generic.HKD (construct, label)
import Data.Generics.Labels ()
import Data.List qualified as List
import Data.Maybe (fromJust)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Time (defaultTimeLocale, formatTime)
import Data.Time.LocalTime qualified as RawTime
import Development.LAMMPS.GitOps.API.Client
import Development.LAMMPS.GitOps.API.Types (JobDescriptor (..), RepoApi (..))
import Development.LAMMPS.GitOps.Options (VersionInfo, formatVersionInfo)
import Development.LAMMPS.GitOps.Options qualified as AppOpt
import Development.LAMMPS.GitOps.Types
import Effectful
import Effectful.FileSystem (runFileSystem)
import Effectful.FileSystem.IO (stdin)
import Effectful.FileSystem.Streaming qualified as QE
import Effectful.Log (Log, logAttention_, logInfo)
import Effectful.Log.Extra (LogLevel (..), logInfo_, logTrace, runStdErrLogger)
import Effectful.Reader.Static (Reader, runReader)
import Effectful.Reader.Static.Lens qualified as EffL
import Effectful.Resource (runResource)
import GHC.Bits
import GHC.Generics (Generic, Generically (..))
import GHC.Records (HasField, getField)
import GHC.TypeLits (AppendSymbol, KnownSymbol, symbolVal)
import Graphics.Vty qualified as Vty
import Network.HTTP.Client (ManagerSettings (..), Request (..))
import Network.HTTP.Client.TLS (newTlsManagerWith, tlsManagerSettings)
import Network.HTTP.Types (status404)
import Network.HTTP.Types.Header
import Options.Applicative qualified as Opt
import Path.Tagged.IO (resolveFile')
import Paths_lammps_gitops_daemon (version)
import Servant.Auth.Client (Token (..))
import Servant.Client
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import Text.Printf (printf)
import Text.Show.Extra (tshow)

versionInfo :: AppOpt.VersionInfoQ
versionInfo = AppOpt.versionInfo "LAMMPS GitOps CLI Client" version

defaultMain :: (MonadUnliftIO m) => VersionInfo -> m ()
defaultMain vinfo = do
  envs <- getEnvVars
  defaultMainWith vinfo envs =<< AppOpt.execOptionParserWithVersion vinfo (cliOptsP envs)

defaultMainWith :: (MonadIO m) => VersionInfo -> EnvVars -> CliOpts -> m ()
defaultMainWith vinfo env copts@CliOpts {..} = do
  let logLevel
        | verbose = LogTrace
        | otherwise = LogInfo

  liftIO $ runEff $ runStdErrLogger "cli" logLevel $ runReader apiConfig $ do
    logInfo_ $ T.pack (formatVersionInfo vinfo)
    logTrace "EnvVars" env
    logTrace "Opts" copts
    case cliCmd of
      Schedule repo opts -> schedule repo opts
      Cancel repo opts -> cancel repo opts
      Status repo opts -> listStatus repo opts
      Deploy opts -> deployBins opts

hCfAccessClientId :: HeaderName
hCfAccessClientId = "CF-Access-Client-Id"

hCfAccessClientSecret :: HeaderName
hCfAccessClientSecret = "CF-Access-Client-Secret"

clientEff :: (Reader APIConfig :> es, IOE :> es) => ClientM a -> Eff es a
clientEff act = do
  clientId <- EffL.view #clientID
  clientSecret <- EffL.view #clientSecret
  man <-
    newTlsManagerWith
      tlsManagerSettings
        { managerModifyRequest = \req ->
            pure
              req
                { requestHeaders =
                    maybe
                      id
                      ((:) . (hCfAccessClientSecret,) . T.encodeUtf8)
                      clientSecret
                      $ maybe
                        id
                        ((:) . (hCfAccessClientId,) . T.encodeUtf8)
                        clientId
                      $ List.filter
                        ( not
                            . flip
                              elem
                              [ hCfAccessClientId
                              , hCfAccessClientSecret
                              ]
                            . fst
                        )
                      $ requestHeaders req
                }
        }
  base <- either throwM pure . parseBaseUrl . T.unpack =<< EffL.view #endpoint
  let env = mkClientEnv man base
  either throwM pure =<< liftIO (runClientM act env)

deployBins ::
  (Reader APIConfig :> es, Log :> es, IOE :> es) =>
  DeployOpts ->
  Eff es ()
deployBins DeployOpts {..} = do
  let depA = restClient ^. #deployApi
      dep = depA ^. #deploy
  logInfo_ $
    "Deploying binaries from: " <> maybe "<stdin>" T.pack input <> "..."
  runFileSystem $ runResource $ do
    src <- withEffToIO $ \unlift -> do
      hoist unlift <$> case input of
        Nothing -> pure $ QE.hGetContents stdin
        Just fp -> QE.readFile <$> resolveFile' fp
    void $
      clientEff $
        dep (Token $ T.encodeUtf8 deployToken) src
  logInfo_ "Done!"

listStatus ::
  (Log :> es, Reader APIConfig :> es, IOE :> es) =>
  RepoName ->
  StatusOpts ->
  Eff es ()
listStatus repos@RepoName {..} StatusOpts {..} = do
  case target of
    Nothing -> do
      logInfo_ $ "Listing status of all running jobs of " <> tshow repos
      jobs <- clientEff listAllJobs
      if
          | json -> liftIO $ LBS.putStrLn $ A.encode jobs
          | null jobs -> logInfo_ "No job(s) found."
          | otherwise -> void $ listMain jobs
      logInfo_ "Done."
    Just (Pull n) -> do
      logInfo_ $
        "Listing status of all running jobs of Pull Req #"
          <> tshow n
          <> " of "
          <> tshow repos
      jobs <- clientEff $ pullApi n // view #listPullJobs
      if
          | json -> liftIO $ LBS.putStrLn $ A.encode jobs
          | null jobs -> logInfo_ "No job(s) found."
          | otherwise -> void $ listMain jobs
      logInfo_ "Done."
    Just (CommitJ hash) -> do
      logInfo_ $
        "Listing status of a job for commit "
          <> T.take 7 (coerce hash)
          <> " of "
          <> tshow repos
      aJob <- try $ clientEff $ commitApi hash // view #getCommitJob
      case aJob of
        Left (FailureResponse _ rsp)
          | responseStatusCode rsp == status404 ->
              if json
                then liftIO $ LBS.putStrLn $ A.encode (Nothing @A.Value)
                else logInfo_ "No job is scheduled."
        Left err -> do
          logAttention_ $ "Unexpected Http Exception: " <> T.pack (displayException err)
          liftIO exitFailure
        Right st@JobDescriptor {pull = PullRequest {title, pullNumber}, job = Job {..}}
          | json -> liftIO $ LBS.putStrLn $ A.encode st
          | otherwise -> do
              logInfo_ $ "Commit: " <> coerce commit.commit
              logInfo_ $ "PR: #" <> prettyPull pullNumber <> ": " <> title
              logInfo_ $ "Status: " <> tshow status
              zone <- liftIO RawTime.getCurrentTimeZone
              let fmtTime =
                    T.pack
                      . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                      . RawTime.utcToZonedTime zone
              logInfo_ $ "Created At: " <> fmtTime scheduledAt
              logInfo_ $ "Started At: " <> maybe "N/A" fmtTime startedAt
              logInfo_ $ "Started At: " <> maybe "N/A" fmtTime finishedAt
              logInfo_ "Done"
  where
    RepoApi {..} = (restClient ^. #repoApi) repoOwner repoName
    listMain ls = do
      cfg <- liftIO Vty.standardIOConfig
      vty <- liftIO $ Vty.mkVty cfg
      zone <- liftIO RawTime.getCurrentTimeZone
      liftIO $ Brick.customMain vty (Vty.mkVty cfg) Nothing (statusBirck zone) ls
    statusBirck zone =
      Brick.App
        { appStartEvent = pure ()
        , appHandleEvent = handleEvt
        , appDraw = drawUi zone
        , appChooseCursor = Brick.neverShowCursor
        , appAttrMap = const customAttr
        }
    drawUi zone jobs =
      [ Brick.vBox
          [ mkStatusTable zone jobs
              & Brick.viewport (Proxy @"Table") Brick.Both
          , Brick.withAttr infoBarA $
              Brick.strWrap "[q/Esc]: Quit  [←↓↑→]: Scroll  [SPACE/PgDn]: Next Page  [PgUp]: Prev Page  [Home]: Top  [End]: Bottom"
          ]
      ]
    prettyStatus Held = Brick.withAttr idleA $ Brick.str "Held"
    prettyStatus Queued = Brick.withAttr idleA $ Brick.str "Queued"
    prettyStatus Running = Brick.withAttr (activeA <> italicA) $ Brick.str "Running"
    prettyStatus Successed = Brick.withAttr finishedA $ Brick.str "Successed"
    prettyStatus Aborted = Brick.withAttr fatalA $ Brick.str "Aborted"
    prettyStatus Cancelled = Brick.withAttr fatalA $ Brick.str "Cancelled"
    mkStatusTable zone jobs =
      let hdr = map (Brick.withAttr headerA . Brick.str) ["Commit", "Pull Req", "Status", "Created", "Started", "Finished"]
          body = flip map jobs $ \JobDescriptor {pull = PullRequest {pullNumber, title}, job = Job {..}} ->
            [ Brick.hLimit 7 $
                Brick.txt $
                  T.take 7 commit.commit.getCommitHash
            , Brick.txt $ "#" <> prettyPull pullNumber <> ": " <> title
            , Brick.hLimitPercent 80 $ prettyStatus status
            , Brick.str $
                formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" $
                  RawTime.utcToZonedTime zone scheduledAt
            , Brick.str $
                maybe
                  "N/A"
                  ( formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                      . RawTime.utcToZonedTime zone
                  )
                  startedAt
            , Brick.str $
                maybe
                  "N/A"
                  ( formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
                      . RawTime.utcToZonedTime zone
                  )
                  finishedAt
            ]
       in Brick.table (hdr : body)
            & Brick.setDefaultColAlignment Brick.AlignLeft
            & Brick.alignRight 0
            & Brick.renderTable

    scroll = viewportScroll $ Proxy @"Table"

    handleEvt (AppEvent msg) = id .= msg
    handleEvt (VtyEvent (Vty.EvKey (Vty.KChar (C.toLower -> 'q')) _)) =
      Brick.halt
    handleEvt (VtyEvent (Vty.EvKey (Vty.KChar ' ') [Vty.MShift])) =
      vScrollBy scroll (-1)
    handleEvt (VtyEvent (Vty.EvKey (Vty.KChar ' ') _)) =
      vScrollBy scroll 1
    handleEvt (VtyEvent (Vty.EvKey Vty.KUp _)) =
      vScrollBy scroll (-1)
    handleEvt (VtyEvent (Vty.EvKey Vty.KDown _)) =
      vScrollBy scroll 1
    handleEvt (VtyEvent (Vty.EvKey Vty.KLeft _)) =
      hScrollBy scroll (-1)
    handleEvt (VtyEvent (Vty.EvKey Vty.KRight _)) =
      hScrollBy scroll 1
    handleEvt (VtyEvent (Vty.EvKey Vty.KPageDown _)) =
      vScrollPage scroll Brick.Down
    handleEvt (VtyEvent (Vty.EvKey Vty.KPageUp _)) =
      vScrollPage scroll Brick.Up
    handleEvt (VtyEvent (Vty.EvKey Vty.KHome _)) =
      vScrollToBeginning scroll
    handleEvt (VtyEvent (Vty.EvKey Vty.KEnd _)) =
      vScrollToEnd scroll
    handleEvt _ = pure ()

customAttr :: Brick.AttrMap
customAttr =
  Brick.attrMap
    (Vty.black `Brick.on` Vty.white)
    [ (idleA, Brick.fg gray & #attrStyle .~ Vty.SetTo Vty.bold)
    , (activeA, Brick.fg Vty.green & #attrStyle .~ Vty.SetTo (Vty.bold .|. Vty.italic))
    , (italicA, Vty.defAttr & #attrStyle .~ Vty.SetTo Vty.italic)
    , (finishedA, Brick.fg Vty.blue & #attrStyle .~ Vty.SetTo Vty.bold)
    , (warnA, Brick.fg Vty.yellow & #attrStyle .~ Vty.SetTo Vty.bold)
    , (infoA, Brick.fg Vty.blue & #attrStyle .~ Vty.SetTo Vty.bold)
    , (fatalA, Brick.fg Vty.red & #attrStyle .~ Vty.SetTo Vty.bold)
    , (timeA, Brick.fg gray & #attrStyle .~ Vty.SetTo Vty.bold)
    , (infoBarA, Vty.black `Brick.on` Vty.green & #attrStyle .~ Vty.SetTo Vty.bold)
    , (headerA, Brick.fg Vty.black & #attrStyle .~ Vty.SetTo Vty.bold)
    ]

fatalA :: Brick.AttrName
fatalA = Brick.attrName "fatal"

timeA :: Brick.AttrName
timeA = Brick.attrName "time"

warnA :: Brick.AttrName
warnA = Brick.attrName "warn"

infoA :: Brick.AttrName
infoA = Brick.attrName "info"

infoBarA :: Brick.AttrName
infoBarA = Brick.attrName "infoBar"

activeA :: Brick.AttrName
activeA = Brick.attrName "Active"

italicA :: Brick.AttrName
italicA = Brick.attrName "italic"

idleA :: Brick.AttrName
idleA = Brick.attrName "Idle"

finishedA :: Brick.AttrName
finishedA = Brick.attrName "Finished"

headerA :: Brick.AttrName
headerA = Brick.attrName "header"

gray :: Vty.Color
gray = Vty.rgbColor @Int 0x55 0x55 0x55

newtype StatusListState = StatusListState {statuses :: [Job]}
  deriving (Generic)
  deriving (Semigroup, Monoid) via Generically StatusListState

schedule :: (Log :> es, Reader APIConfig :> es, IOE :> es) => RepoName -> JobSource -> Eff es ()
schedule RepoName {..} (FromPRCommit pull hash) = do
  logInfo_ $ "Scheduling job for commit " <> T.take 7 (coerce hash) <> " in PR #" <> tshow pull
  src <- clientEff $ restClient // view #repoApi /: repoOwner /: repoName // view #commitApi /: hash // view #scheduleCommitJob /: pull
  logInfo "Job scheduled" src
schedule RepoName {..} (FromPull pull) = do
  logInfo_ $ "Scheduling job for PR #" <> tshow pull
  src <- clientEff $ restClient // view #repoApi /: repoOwner /: repoName // view #pullApi /: pull // view #schedulePullNewestCommitJob
  logInfo "Job scheduled" src

cancel :: (Log :> es, Reader APIConfig :> es, IOE :> es) => RepoName -> CancelOpts -> Eff es ()
cancel RepoName {..} CancelOpts {target = Pull pull, cancelAll}
  | cancelAll = do
      logInfo_ $ "Cancelling all jobs of PR #" <> prettyPull pull <> "..."
      jobs <-
        clientEff $
          restClient // view #repoApi /: repoOwner /: repoName // view #pullApi /: pull // view #cancelAllPullJobs
      if null jobs
        then logAttention_ "No jobs killed."
        else do
          logInfo_ $ tshow (length jobs) <> " job(s) killed:"
          forM_ jobs $ \JobDescriptor {job = Job {..}} ->
            logInfo_ $ "- " <> T.take 7 (coerce commit.commit) <> ": " <> commitMessage
  | otherwise = do
      logInfo_ $ "Cancelling the latest job of PR #" <> prettyPull pull <> "..."
      eith <-
        try $
          clientEff $
            restClient
              // view #repoApi
              /: repoOwner
              /: repoName
              // view #pullApi
              /: pull
              // view #cancelMostRecentPullJob
      reportSingleJobCancel ("pull #" <> prettyPull pull) eith
cancel RepoName {..} CancelOpts {target = CommitJ hash} = do
  logInfo_ $ "Killing the job for commit: " <> coerce hash
  eith <-
    try $
      clientEff $
        restClient
          // view #repoApi
          /: repoOwner
          /: repoName
          // view #commitApi
          /: hash
          // view #cancelCommitJob
  reportSingleJobCancel ("commit " <> T.take 7 (coerce hash)) eith

reportSingleJobCancel ::
  (Log :> es, IOE :> es) =>
  Text ->
  Either ClientError JobDescriptor ->
  Eff es ()
reportSingleJobCancel ctx = \case
  Left (FailureResponse _ rsp)
    | responseStatusCode rsp == status404 ->
        logAttention_ $ "No job for " <> ctx <> " found. No job cancelled."
  Left err -> do
    logAttention_ $ "Unexpected exception: " <> T.pack (displayException err)
    liftIO exitFailure
  Right JobDescriptor {job = Job {..}} -> do
    logInfo_ "Job killed successfully:"
    logInfo_ $ "- Commit: " <> coerce commit.commit
    logInfo_ $ "- Commit Message: " <> commitMessage

prettyPull :: IssueNumber -> Text
prettyPull = T.pack . printf "%03d" . view #getIssueNumber

data CliOpts = CliOpts {apiConfig :: APIConfig, cliCmd :: CliCmd, verbose :: Bool}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

cliOptsP :: EnvVars -> Opt.ParserInfo CliOpts
cliOptsP env =
  Opt.info p $ Opt.progDesc "LAMMPS GitOps API Client"
  where
    p = do
      apiConfig <- apiConfigP env
      verbose <-
        Opt.switch $
          Opt.long "verbose"
            <> Opt.short 'V'
            <> Opt.help "Verbose logging"
      cliCmd <- cmdsP
      pure CliOpts {..}
    cmdsP =
      Opt.hsubparser
        ( Opt.command "schedule" (scheduleP env)
            <> Opt.command "cancel" (cancelP env)
            <> Opt.command "status" (statusP env)
            <> Opt.commandGroup "Job Commands:"
            <> Opt.metavar "Job Commands"
        )
        <|> Opt.hsubparser
          ( Opt.command "deploy" (deployP env)
              <> Opt.commandGroup "CI/CD Commands:"
              <> Opt.metavar "CI/CD Commands"
          )

data CliCmd
  = Schedule RepoName JobSource
  | Cancel RepoName CancelOpts
  | Status RepoName StatusOpts
  | Deploy DeployOpts
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data JobSource
  = FromPRCommit IssueNumber CommitHash
  | FromPull IssueNumber
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

jobSourceP :: EnvVars -> Opt.Parser JobSource
jobSourceP env = do
  pull <- pullArgP
  mcommit <- optional $ CommitHash <$> withDefault @"GITHUB_SHA" env commitArgP
  pure $ case mcommit of
    Nothing -> FromPull pull
    Just comm -> FromPRCommit pull comm

scheduleP :: EnvVars -> Opt.ParserInfo CliCmd
scheduleP env = Opt.info p $ Opt.progDesc "Schedule jobs"
  where
    p = Schedule <$> repoOpt env <*> jobSourceP env

data CancelOpts = CancelOpts {target :: JobTarget, cancelAll :: Bool}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

cancelP :: EnvVars -> Opt.ParserInfo CliCmd
cancelP env = Opt.info p $ Opt.progDesc "Cancel jobs"
  where
    p = Cancel <$> repoOpt env <*> cancelOptP
    cancelOptP = do
      target <- jobTargetP
      cancelAll <-
        Opt.switch $
          Opt.long "all"
            <> Opt.short 'A'
            <> Opt.help "Cancel all matching jobs (effective only if pull cancell mode)"
      pure CancelOpts {..}

data StatusOpts = StatusOpts
  { target :: Maybe JobTarget
  , json :: Bool
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

statusP :: EnvVars -> Opt.ParserInfo CliCmd
statusP env = Opt.info p $ Opt.progDesc "Get job statuses"
  where
    p = Status <$> repoOpt env <*> statusOptsP
    statusOptsP = do
      target <- Opt.optional jobTargetP
      json <-
        Opt.switch $
          Opt.long "json" <> Opt.short 'J' <> Opt.help "Dumps status as a json to STDOUT"
      pure StatusOpts {..}

deployP :: EnvVars -> Opt.ParserInfo CliCmd
deployP env = Opt.info p $ Opt.progDesc "Deploy binaries and shared objects"
  where
    p =
      Deploy <$> do
        input <-
          Opt.option
            ( Opt.eitherReader $ \case
                "" -> Left "Empty input path given"
                "-" -> Right Nothing
                fp -> Right $ Just fp
            )
            $ Opt.long "input"
              <> Opt.short 'I'
              <> Opt.metavar "PATH"
              <> Opt.help "The path to the deploy source. Use - to read from stdin."
        deployToken <-
          withDefault @"LAMMOPS_DEPLOY_KEY" env $
            Opt.strOption $
              Opt.long "deploy-key"
                <> Opt.short 'K'
                <> Opt.metavar "BEARER"
                <> Opt.help "Bearer Token for deploy. Uses LAMMOPS_DEPLOY_KEY env var as a default if set."
        pure DeployOpts {..}

data JobTarget
  = Pull !IssueNumber
  | CommitJ !CommitHash
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

repoOpt :: EnvVars -> Opt.Parser RepoName
repoOpt env =
  withDefault' @"GITHUB_REPOSITORY" (fromJust . parseRepo . T.unpack) env $
    Opt.option (Opt.maybeReader parseRepo) $
      Opt.long "repo"
        <> Opt.short 'R'
        <> Opt.metavar "OWNER/REPO"
        <> Opt.help "Repository name"

jobTargetP :: Opt.Parser JobTarget
jobTargetP = Pull <$> pullArgP <|> CommitJ . CommitHash <$> commitArgP

pullArgP :: Opt.Parser IssueNumber
pullArgP = Opt.option Opt.auto (Opt.metavar "NUM" <> Opt.long "pull" <> Opt.short 'P' <> Opt.help "Pull-request number")

commitArgP :: Opt.Parser Text
commitArgP = Opt.strOption (Opt.metavar "HASH" <> Opt.long "commit" <> Opt.long "sha" <> Opt.short 's' <> Opt.help "Commit Hash")

data APIConfig = APIConfig
  { endpoint :: Text
  , clientID, clientSecret :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DeployOpts = DeployOpts {input :: Maybe FilePath, deployToken :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data EnvVars = EnvVars
  { kLAMMOPS_DAEMON_URI :: Maybe Text
  , kLAMMOPS_CF_CLIENT_ID :: Maybe Text
  , kLAMMOPS_CF_CLIENT_SECRET :: Maybe Text
  , kLAMMOPS_DEPLOY_KEY :: Maybe Text
  , kGITHUB_REPOSITORY :: Maybe Text
  , kGITHUB_SHA :: Maybe Text
  , kGITHUB_HEAD_REF :: Maybe Text
  , kGITHUB_EVENT_PATH :: Maybe Text
  }
  deriving (Show, Eq, Ord, Generic)

envVarsOpts :: A.Options
envVarsOpts = A.defaultOptions {A.fieldLabelModifier = drop 1}

instance ToJSON EnvVars where
  toJSON = A.genericToJSON envVarsOpts

apiConfigP :: EnvVars -> Opt.Parser APIConfig
apiConfigP envs = do
  endpoint <-
    withDefault @"LAMMOPS_DAEMON_URI" envs $
      Opt.strOption $
        Opt.long "endpoint"
          <> Opt.short 'E'
          <> Opt.metavar "URI"
          <> Opt.help "URL to the daemon server. Defaulted to LAMMOPS_DAEMON_URI env var if set."
  clientID <-
    Opt.optional $
      withDefault @"LAMMOPS_CF_CLIENT_ID" envs $
        Opt.strOption $
          Opt.long "client-id"
            <> Opt.short 'T'
            <> Opt.help "Client ID. Defaulted to LAMMOPS_CF_CLIENT_ID env var if set."
  clientSecret <-
    Opt.optional $
      withDefault @"LAMMOPS_CF_CLIENT_SECRET" envs $
        Opt.strOption $
          Opt.long "client-id"
            <> Opt.short 'S'
            <> Opt.help "Client Secret. Defaulted to LAMMOPS_CF_CLIENT_SECRET env var if set."
  pure APIConfig {..}

withDefault :: forall f env. (KnownSymbol f, HasField (AppendSymbol "k" f) env (Maybe Text)) => env -> Opt.Parser Text -> Opt.Parser Text
withDefault = withDefault' @f id

withDefault' ::
  forall f a env.
  (KnownSymbol f, HasField (AppendSymbol "k" f) env (Maybe Text)) =>
  (Text -> a) ->
  env ->
  Opt.Parser a ->
  Opt.Parser a
withDefault' f env p =
  p
    <|> maybe
      ( Opt.option
          (Opt.readerError $ "No cli option or env var `" <> symbolVal @f Proxy <> "' is specified")
          Opt.internal
      )
      (pure . f)
      (getField @(AppendSymbol "k" f) env)

getEnvVars :: (MonadIO m) => m EnvVars
getEnvVars = liftIO $ do
  runIdentity . construct
    <$> btraverseC @((~) (Maybe Text))
      ( \(Const l) ->
          Identity . fmap T.pack <$> lookupEnv (drop 1 l)
      )
      (label @EnvVars)
