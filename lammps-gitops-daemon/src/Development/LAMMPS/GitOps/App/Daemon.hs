{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.App.Daemon (
  defaultMain,
  versionInfo,
  defaultMainWith,
  DaemonOptions (..),
  daemonOptionsP,
  DaemonConfig (..),
  withRootDir,
  dummyDaemonConfig,
  DaemonEnv (..),
  runDaemon,
  runDaemonWith,
  withDaemonEnv,
  generateJWKIfMissing,
  daemonApp,
) where

import Control.Applicative
import Control.Lens ((^.))
import Control.Monad.IO.Class (MonadIO (liftIO))
import Crypto.JOSE.JWK (JWK)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Char8 qualified as BS8
import Data.Function ((&))
import Data.List (find)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Reflection (Given, give)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Yaml qualified as Y
import Development.LAMMPS.GitOps.API.Server
import Development.LAMMPS.GitOps.API.Types
import Development.LAMMPS.GitOps.App.Daemon.Model (migrateUserDb)
import Development.LAMMPS.GitOps.Options (VersionInfo, execOptionParserWithVersion, formatVersionInfo)
import Development.LAMMPS.GitOps.Options qualified as AppOpt
import Development.LAMMPS.GitOps.Paths
import Development.LAMMPS.GitOps.Types (GitHubConfig (..), migrateGitHubDb)
import Development.LAMMPS.GitOps.Webhook.Server
import Development.LAMMPS.GitOps.Workflow.Config (SchedulerType (..))
import Effectful (Eff, IOE, MonadUnliftIO, runEff)
import Effectful qualified as Eff
import Effectful.Alias
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Concurrent.TimedResource (Expiration, runExpiration)
import Effectful.Database.Beam.Sqlite (SqlitePool, withSqlPool)
import Effectful.Environment (Environment, runEnvironment)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.Log
import Effectful.Log.Extra (withStderrLogger)
import Effectful.Network.GitHub.Apps (APITokenConfig (..), APITokens)
import Effectful.Network.GitHub.Apps qualified as EffGH
import Effectful.Process.Typed (TypedProcess, runTypedProcess)
import Effectful.Random.Static (Random, evalRandom, getStdGen, runRandom)
import Effectful.Reader.Static (Reader, ask, runReader)
import Effectful.Reader.Static.Lens qualified as EffL
import Effectful.Servant (runWarpServerSettingsContext)
import Effectful.Time (Clock, runClock)
import GHC.Generics (Generic)
import GitHub.REST.Auth (loadSigner)
import Network.HTTP.Types.Status (statusCode)
import Network.Wai (Request (..))
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Handler.Warp qualified as Warp
import Network.Wai.Logger (showSockAddr)
import Options.Applicative qualified as Opt
import Path.Tagged (Abs, Dir, File, PathTo (untagPath), fromAbsDir, fromAbsFile, parent)
import Path.Tagged.IO
import Paths_lammps_gitops_daemon (version)
import Servant
import Servant.Auth.Server (CookieSettings, JWTSettings, defaultJWTSettings, readKey, writeKey)
import Servant.Auth.Server.Internal.ConfigTypes (defaultCookieSettings)
import Servant.GitHub.Webhook (gitHubKey)
import System.Posix (setFileMode)
import Text.Show.Extra (tshow)
import Web.JWT (EncodeSigner)

data DaemonConfig = DaemonConfig
  { host :: !String
  , port :: !Port
  , pushDeploy :: !(Maybe Bool)
  , webhook :: !WebhookConfig
  , jwkPath :: !(Maybe FilePath)
  , github :: !GitHubConfig
  , scheduler :: !SchedulerType
  , rootDir :: !(Maybe (PathTo EtcDir Abs Dir))
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

withRootDir :: DaemonConfig -> ((Given (PathTo 'EtcDir Abs Dir)) => r) -> r
withRootDir dc = give (fromMaybe defaultEtcDir dc.rootDir)

dummyDaemonConfig :: DaemonConfig
dummyDaemonConfig =
  DaemonConfig
    { host = "https://api.example.com"
    , port = 9291
    , pushDeploy = Just False
    , webhook = WebhookConfig {secret = "<Webhook Secret Here>"}
    , jwkPath = Nothing
    , github =
        GitHubConfig
          { ownerID = 12345
          , appID = 123456789
          , appName = Just "LAMMPS GitOps Tool"
          , repos = NE.singleton EffGH.Repository {owner = "<Owner>", name = "<REPO>"}
          }
    , scheduler = UgeDrmaa2
    , rootDir = Nothing
    }

newtype WebhookConfig = WebhookConfig {secret :: T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

type DaemonAPI = ToServantApi RestApi :<|> "webhook" :> "github" :> ToServantApi WebhookApi

data DaemonEnv = DaemonEnv
  { logger :: !Logger
  , jwk :: !JWK
  , pushDeploy :: !Bool
  , githubSigner :: !EncodeSigner
  , githubConfig :: !GitHubConfig
  , apiToken :: !APITokens
  , userDbPool, githubDbPool :: !SqlitePool
  , scheduler :: !SchedulerType
  }
  deriving (Generic)

newtype DaemonOptions = DaemonOptions {config :: Maybe FilePath}
  deriving (Show, Eq, Ord, Generic)

versionInfo :: AppOpt.VersionInfoQ
versionInfo = AppOpt.versionInfo "LAMMPS GitOps Daemon" version

defaultMain :: (MonadUnliftIO m) => VersionInfo -> m ()
defaultMain vinfo =
  defaultMainWith vinfo
    =<< execOptionParserWithVersion vinfo daemonOptionsP

defaultMainWith ::
  (MonadUnliftIO m) =>
  VersionInfo ->
  DaemonOptions ->
  m ()
defaultMainWith vinfo DaemonOptions {..} = liftIO $ runEff $ do
  cfgPath <- maybe (give defaultEtcDir getDaemonConfigYaml) resolveFile' config
  cfg <- liftIO $ Y.decodeFileThrow $ fromAbsFile cfgPath
  runDaemon vinfo cfg

daemonOptionsP :: Opt.ParserInfo DaemonOptions
daemonOptionsP = Opt.info p $ Opt.progDesc "LAMMPS GitOps Daemon Server"
  where
    p = do
      config <-
        optional $
          Opt.strOption $
            Opt.long "config"
              <> Opt.short 'c'
              <> Opt.metavar "FILE"
              <> Opt.help "The path to the config yaml (default: ${XDG_CONFIG_HOME}/config.yml)"
      pure DaemonOptions {..}

runDaemon :: (IOE ∈ es) => VersionInfo -> DaemonConfig -> Eff es ()
runDaemon vinfo cfg = withStderrLogger "daemon" $ \logger -> do
  runLog "daemon" logger LogInfo $
    logInfo_ $
      T.pack $
        formatVersionInfo vinfo
  withDaemonEnv cfg logger $ \env -> do
    runDaemonWith cfg env

generateJWKIfMissing ::
  ( IOE ∈ es
  , Reader DaemonConfig ∈ es
  , Log ∈ es
  , Given (PathTo 'EtcDir Abs Dir)
  ) =>
  Eff es (PathTo JWKFile Abs File)
generateJWKIfMissing = do
  jwkFP <- maybe getJWKFile resolveFile' =<< EffL.view #jwkPath
  there <- doesFileExist jwkFP
  if there
    then logInfo_ $ "JWK found: " <> T.pack (fromAbsFile jwkFP)
    else do
      logInfo_ "No JWK found. Generating..."
      liftIO $ do
        createDirIfMissing True $ parent jwkFP
      liftIO $ setFileMode (fromAbsDir $ parent jwkFP) 0o700
      liftIO $ do
        writeKey (fromAbsFile jwkFP)
        setFileMode (fromAbsFile jwkFP) 0o500
      logInfo_ $ "JWK Saved to: " <> T.pack (fromAbsFile jwkFP)
  pure jwkFP

withDaemonEnv ::
  (IOE ∈ es) =>
  DaemonConfig ->
  Logger ->
  (DaemonEnv -> Eff es a) ->
  Eff es a
withDaemonEnv dc logger k = withRootDir dc $ runLog "daemon" logger LogTrace $ do
  jwkFP <- runReader dc generateJWKIfMissing

  logInfo_ $ "Reading JWK file from: " <> T.pack (fromAbsFile jwkFP)
  jwk <- liftIO $ readKey $ fromAbsFile jwkFP
  gen <- liftIO getStdGen
  runConcurrent $ runRandom gen $ migrateUserDb >> migrateGitHubDb
  userDbPath <- getUserDbFile
  githubDbPath <- getGitHubDbFile
  withSqlPool (untagPath userDbPath) $ \userDbPool ->
    withSqlPool (untagPath githubDbPath) $ \githubDbPool -> runConcurrent $ runClock $ runExpiration $ do
      githubSigner <- liftIO . loadSigner . fromAbsFile =<< getGitHubAppKeyFile
      let githubConfig = dc.github
          scheduler = dc.scheduler
          apiTokenConfig =
            APITokenConfig
              { github =
                  EffGH.GitHubConfig
                    { privKey = githubSigner
                    , appID = dc.github.appID
                    , appName = fromMaybe "LAMMPS GitOps" dc.github.appName
                    }
              , repos = dc.github.repos
              }
      apiToken <- EffGH.newAPITokens apiTokenConfig
      let pushDeploy = fromMaybe False dc.pushDeploy
      Eff.inject $ k DaemonEnv {..}

getSource :: Request -> Maybe BS8.ByteString
getSource req = addr
  where
    maddr = find (\x -> fst x `elem` ["x-real-ip", "x-forwarded-for"]) hdrs
    addr = fmap snd maddr
    hdrs = requestHeaders req

getSourceFromSocket :: Request -> BS8.ByteString
getSourceFromSocket = BS8.pack . showSockAddr . remoteHost

getSourceFromFallback :: Request -> BS8.ByteString
getSourceFromFallback req = fromMaybe (getSourceFromSocket req) $ getSource req

runDaemonWith ::
  (IOE ∈ es) =>
  DaemonConfig ->
  DaemonEnv ->
  Eff es ()
runDaemonWith dc@DaemonConfig {..} env = withRootDir dc $
  runLog "daemon" (env ^. #logger) LogTrace $
    do
      let setts =
            Warp.defaultSettings
              & Warp.setPort port
              & Warp.setLogger
                ( \req status mlen -> do
                    let path = rawPathInfo req <> rawQueryString req
                        mr = requestHeaderReferer req
                        mua = requestHeaderUserAgent req
                        logStr =
                          T.decodeUtf8 (getSourceFromFallback req)
                            <> " - "
                            <> "-"
                            <> " \""
                            <> T.decodeUtf8 (requestMethod req)
                            <> " "
                            <> T.decodeUtf8 path
                            <> "\" "
                            <> tshow (statusCode status)
                            <> " "
                            <> maybe "-" tshow mlen
                            <> " \""
                            <> maybe "" T.decodeUtf8 mr
                            <> "\" \""
                            <> maybe "" T.decodeUtf8 mua
                            <> "\""
                    runLogT "daemon" (env ^. #logger) LogInfo $
                      localDomain "access" $
                        logInfo_ logStr
                )
      logInfo_ $ "Server listen at port " <> tshow port
      gen <- getStdGen
      runTypedProcess $
        runConcurrent $
          runEnvironment $
            evalRandom gen $
              runClock $
                runExpiration $
                  runFileSystem $
                    runReader env $
                      runWarpServerSettingsContext
                        @DaemonAPI
                        setts
                        ( GitHubSecret (gitHubKey $ pure $ T.encodeUtf8 $ webhook ^. #secret)
                            :. defaultJWTSettings (env ^. #jwk)
                            :. defaultCookieSettings
                            :. EmptyContext
                        )
                        daemonApp

withApiEnv :: (Reader DaemonEnv ∈ es) => Eff (Reader APIServerEnv ': es) a -> Eff es a
withApiEnv act = do
  DaemonEnv {..} <- ask
  runReader APIServerEnv {..} act

withWebhookEnv :: (Reader DaemonEnv ∈ es) => Eff (Reader WebhookEnv ': es) a -> Eff es a
withWebhookEnv act = do
  DaemonEnv {..} <- ask
  runReader WebhookEnv {..} act

daemonApp ::
  ( Reader DaemonEnv ∈ es
  , Log ∈ es
  , IOE ∈ es
  , FileSystem ∈ es
  , Random ∈ es
  , Concurrent ∈ es
  , TypedProcess ∈ es
  , Expiration ∈ es
  , Environment ∈ es
  , Clock ∈ es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  ServerT DaemonAPI (Eff es)
daemonApp =
  hoistServerWithContext
    (Proxy @(ToServantApi RestApi))
    (Proxy @'[JWTSettings, CookieSettings])
    withApiEnv
    apiServer
    :<|> hoistServerWithContext
      (Proxy @(ToServantApi WebhookApi))
      (Proxy @'[GitHubSecret])
      withWebhookEnv
      webhookApp
