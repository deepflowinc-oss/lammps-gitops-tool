{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.LAMMPS.GitOps.App.Admin (
  defaultMain,
  versionInfo,
  defaultMainWith,
  UtilOpts (..),
  utilOptsP,
  UtilCmd (..),
  utilCmdP,
  UserCmd (..),
  DeployOpts (..),
  RegisterUserOpts (..),
  UnregisterUserOpts (..),
  userCmdP,
  ControllerException (..),
) where

import Control.Applicative (optional)
import Control.Exception.Safe
import Control.Lens ((^.))
import Control.Monad (forM, forM_, replicateM, unless, when)
import Control.Monad.Morph (hoist)
import Crypto.Nonce qualified as Nonce
import Data.Bifunctor qualified as Bi
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Reflection (Given, give)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (addUTCTime)
import Data.Vector qualified as V
import Data.Yaml qualified as Y
import Database.Beam ((==.))
import Database.Beam qualified as Beam
import Development.LAMMPS.GitOps.App.Daemon (DaemonConfig, dummyDaemonConfig, generateJWKIfMissing)
import Development.LAMMPS.GitOps.App.Daemon.Model
import Development.LAMMPS.GitOps.Deployment (defaultDeployment, deployFrom)
import Development.LAMMPS.GitOps.Options (VersionInfo, VersionInfoQ, execOptionParserWithVersion, formatVersionInfo)
import Development.LAMMPS.GitOps.Options qualified as AppOpt
import Development.LAMMPS.GitOps.Paths
import Development.LAMMPS.GitOps.Types (dummyRepoConfig, migrateGitHubDb)
import Effectful
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Database.Beam.Sqlite (Sqlite, SqliteDb (..), runSqlite, transactExclusive)
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.Streaming qualified as F
import Effectful.FileSystem.Streaming qualified as QE
import Effectful.FileSystem.Tagged (createDirIfMissing, doesFileExist, setFileMode)
import Effectful.Log.Extra
import Effectful.Network.Http (responseBody, runSimpleHttp, withResponseStream)
import Effectful.Random.Static (Random, evalRandom, newStdGen, uniformR)
import Effectful.Reader.Static (runReader)
import Effectful.Resource (runResource)
import Effectful.Time (Clock, getCurrentTime, runClock)
import GHC.Generics (Generic)
import Options.Applicative qualified as Opt
import Path.Tagged (Abs, Dir, PathTo, fromAbsFile, parent, parseAbsDir, relfile, untagPath, (</>))
import Path.Tagged.IO (resolveFile')
import Paths_lammps_gitops_daemon (version)
import Servant.Auth.Server (defaultJWTSettings, verifyJWT)
import Servant.Auth.Server qualified as Auth
import Text.Show.Extra (tshow)

versionInfo :: VersionInfoQ
versionInfo = AppOpt.versionInfo "LAMMPS admin utility" version

defaultMain :: (MonadIO m) => VersionInfo -> m ()
defaultMain vinfo = do
  defaultMainWith vinfo =<< liftIO (execOptionParserWithVersion vinfo utilOptsP)

defaultMainWith :: (MonadIO m) => VersionInfo -> UtilOpts -> m ()
defaultMainWith vinfo env@UtilOpts {..} = give rootDir $ liftIO $ do
  gen <- newStdGen
  runEff $
    runClock $
      runConcurrent $
        evalRandom gen $
          runFileSystem $
            runConcurrent $
              runReader env $
                runStdErrLogger "controller" LogInfo $
                  do
                    logInfo_ $ T.pack $ formatVersionInfo vinfo
                    migrateUserDb
                    case cmd of
                      UserCmd ucmd -> runUserCmd ucmd
                      Deploy opts -> deploy opts
                      Init opts -> initAppData opts

runUserCmd ::
  ( Concurrent :> es
  , Random :> es
  , IOE :> es
  , Log :> es
  , FileSystem :> es
  , Clock :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  UserCmd ->
  Eff es ()
runUserCmd ucmd = do
  db <- liftIO getUserDbFile
  createDirIfMissing True $ parent db
  logInfo_ $ "Using User Database: " <> T.pack (fromAbsFile db)
  runSqlite (DbFile $ untagPath db) $
    case ucmd of
      RegisterUser opts -> registerUser opts
      UnregisterUser opts -> unregisterUser opts
      IssueToken opts -> issueUserToken opts
      RotateToken opts -> rotateUserToken opts
      ListUser -> listUser

deploy ::
  ( IOE :> es
  , Log :> es
  , FileSystem :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  DeployOpts ->
  Eff es ()
deploy DeployOpts {..} = localDomain "deploy" $ do
  src' <- liftIO $ resolveFile' source
  logInfo_ $ "Deploying from: " <> tshow src'
  dep <- liftIO defaultDeployment
  runResource $ do
    deployFrom dep $ QE.readFile src'
  logInfo_ "Deploy Completed."

issueUserToken ::
  ( Log :> es
  , Sqlite :> es
  , Random :> es
  , Concurrent :> es
  , IOE :> es
  , Clock :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  IssueOpts ->
  Eff es ()
issueUserToken IssueOpts {..} = do
  muser <-
    Db.transact $
      Db.selectFirst $
        Beam.lookup_ (userDb ^. #users) $
          UserKey userName
  case muser of
    Nothing -> throwString $ "User not found: " <> T.unpack userName
    Just User {..} -> do
      jwkPath <- getJWKFile
      jwk <- liftIO $ Auth.readKey $ fromAbsFile jwkPath
      expire <- forM expiresInSecond $ \i ->
        addUTCTime (fromIntegral i) <$> getCurrentTime
      tok <-
        either (throwString . show) pure
          =<< signAuthUser jwk expire AuthUser {..}
      logInfo_ $ "User: " <> userName
      logInfo_ $ "Bearer: " <> TE.decodeUtf8 (LBS.toStrict tok)

rotateUserToken ::
  ( Log :> es
  , Sqlite :> es
  , Random :> es
  , Concurrent :> es
  , IOE :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  RotateOpts ->
  Eff es ()
rotateUserToken RotateOpts {..} = do
  au <- rotateUserNonce userName
  jwkPath <- getJWKFile
  jwk <- liftIO $ Auth.readKey $ fromAbsFile jwkPath
  tok <-
    either (throwString . show) pure
      =<< signAuthUser jwk Nothing au
  logInfo_ $ "User Rotated: " <> userName
  logInfo_ $ "Bearer: " <> TE.decodeUtf8 (LBS.toStrict tok)

newtype ControllerException = UserAlreadyRegistered T.Text
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

-- FIXME: Must be moved to random-effectful
element ::
  (Random :> es, Concurrent :> es) => NonEmpty a -> Eff es a
element els = do
  let v = V.fromList $ NE.toList els
  (v V.!) <$> uniformR (0, V.length v - 1)

registerUser ::
  ( Log :> es
  , Sqlite :> es
  , Random :> es
  , Concurrent :> es
  , IOE :> es
  , Clock :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  RegisterUserOpts ->
  Eff es ()
registerUser RegisterUserOpts {..} = do
  logInfo_ $ "Registering: " <> userName
  pass <- case userPass of
    Just pass -> pure $ TE.encodeUtf8 pass
    Nothing -> do
      logInfo_ "Generating Passwords..."
      pw <-
        replicateM 64 $
          element $
            NE.fromList $
              ['0' .. '9']
                ++ ['a' .. 'z']
                ++ ['A' .. 'Z']
                ++ "$%_-:+"
      BS8.pack pw <$ logInfo_ ("Generated Password (store safely): " <> T.pack pw)
  nonce <- liftIO $ Nonce.withGenerator Nonce.nonce128urlT
  authUser <- registerNewUser User {name = userName, nonce, pass}

  logInfo_ $ "User registered: " <> userName
  jwkPath <- getJWKFile
  jwk <- liftIO $ Auth.readKey $ fromAbsFile jwkPath
  logInfo_ $ "Sigining JWT with Key: " <> T.pack (fromAbsFile jwkPath)
  expire <- forM expiresInSecond $ \i ->
    addUTCTime (fromIntegral i) <$> getCurrentTime
  du <-
    either (throwM . userError . show) pure
      =<< signAuthUser jwk expire authUser
  ver <- liftIO $ verifyJWT @AuthUser (defaultJWTSettings jwk) $ LBS.toStrict du
  maybe
    (throwString "Verification failed!")
    ( \AuthUser {nonce = nonce', ..} -> do
        when (name /= userName) $
          throwString $
            "User name mismatched!: (expected, got) = "
              <> "("
              <> T.unpack userName
              <> ", "
              <> T.unpack name
              <> ")"
        when (nonce /= nonce') $
          throwString $
            "User name mismatched!: (expected, got) = "
              <> "("
              <> T.unpack nonce
              <> ", "
              <> T.unpack nonce'
              <> ")"
    )
    ver
  logInfo_ "Verification succeeded."
  logInfo_ $ "Bearer: " <> tshow (LBS.toStrict du)

unregisterUser ::
  (Log :> es, Sqlite :> es, Concurrent :> es, Random :> es) =>
  UnregisterUserOpts ->
  Eff es ()
unregisterUser UnregisterUserOpts {..} = do
  logInfo_ $ "Unregistering: " <> userName
  transactExclusive $ do
    users <- Db.selectMany $ Beam.lookup_ (userDb ^. #users) $ UserKey userName
    when (null users) $
      throwString $
        "No user named `"
          <> T.unpack userName
          <> "' found."
    Db.liftSqliteM $
      Beam.runDelete $
        Beam.delete (userDb ^. #users) $
          \u ->
            u ^. #name ==. Beam.val_ userName

  logInfo_ $ "Unregistered: " <> userName

listUser :: (Log :> es, Sqlite :> es, Concurrent :> es, Random :> es) => Eff es ()
listUser = do
  usrs <-
    Db.notransact $
      Db.selectMany $
        Beam.select $
          Beam.all_ $
            userDb
              ^. #users
  forM_ usrs $ \User {..} -> logInfo_ $ "User: " <> name
  logInfo_ $ "Total " <> tshow (length usrs) <> " user(s) found."

initAppData ::
  ( Log :> es
  , IOE :> es
  , Random :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  InitOpts ->
  Eff es ()
initAppData InitOpts {..} = do
  logInfo_ "Initialising Filesystem..."
  dataD <- getAppDataDir
  cacheD <- getAppCacheDir
  dbsD <- getDbsDir
  cfgD <- getAppConfigDir
  binD <- getAppBinDir
  libD <- getAppLibDir
  runFileSystem $ do
    createDirIfMissing True dbsD
    setFileMode dbsD 0o770
    createDirIfMissing True dataD
    setFileMode dbsD 0o770
    createDirIfMissing True cacheD
    setFileMode dbsD 0o775
    createDirIfMissing True cfgD
    setFileMode dbsD 0o770
    createDirIfMissing True binD
    setFileMode dbsD 0o775
    createDirIfMissing True libD
    setFileMode dbsD 0o770
    createDirIfMissing True $ dataD </> syntaxDir

  logInfo_ "Start Initialising GitOps Environment."
  daemonYamlPath <- maybe getDaemonConfigYaml resolveFile' daemonCfg
  runFileSystem $ do
    thereDaemon <- doesFileExist daemonYamlPath
    unless thereDaemon $ do
      logInfo_ "No daemon config found. generating new config..."
      createDirIfMissing True $ parent daemonYamlPath
      liftIO $ Y.encodeFile (fromAbsFile daemonYamlPath) dummyDaemonConfig
    setFileMode daemonYamlPath 0o660
  let exampleRepoCfg = cfgD </> [relfile|repos.example/123456789.yaml|]
  runFileSystem $ do
    createDirIfMissing True (parent exampleRepoCfg)
    liftIO $ Y.encodeFile (fromAbsFile exampleRepoCfg) dummyRepoConfig
    setFileMode exampleRepoCfg 0o660
  logInfo_ $ "Example config for repository dumped to: " <> T.pack (fromAbsFile exampleRepoCfg)

  dcfg <- Y.decodeFileThrow @_ @DaemonConfig $ fromAbsFile daemonYamlPath
  logInfo_ $ "Daemon config: " <> T.pack (fromAbsFile daemonYamlPath)

  logInfo_ "Initialising JWK..."
  runReader dcfg generateJWKIfMissing
  logInfo_ "Initialising Databses..."
  migrateGitHubDb
  logInfo_ "* GitHub Database Initialised."
  migrateUserDb
  logInfo_ "* User Database Initialised."
  logInfo_ "Initialising auxiliary files..."
  runFileSystem $ forM_ syntaxFiles $ \synFile -> do
    let syntaxFile = dataD </> syntaxDir </> synFile
    synThere <- doesFileExist syntaxFile
    unless synThere $
      runSimpleHttp $
        runResource $
          withResponseStream "https://raw.githubusercontent.com/jgm/skylighting/master/skylighting-core/xml/yaml.xml" $
            \rsp ->
              responseBody rsp & hoist liftIO & F.writeFile syntaxFile

utilOptsP :: Opt.ParserInfo UtilOpts
utilOptsP = Opt.info p $ Opt.progDesc "lammps-daemon utility"
  where
    p = do
      cmd <- utilCmdP
      rootDir <-
        Opt.option (Opt.eitherReader $ Bi.first displayException . parseAbsDir) $
          Opt.long "root"
            <> Opt.short 'R'
            <> Opt.value defaultEtcDir
            <> Opt.showDefault
            <> Opt.help "root directory"
      pure UtilOpts {..}

data UtilOpts = UtilOpts
  { cmd :: !UtilCmd
  , rootDir :: !(PathTo EtcDir Abs Dir)
  }
  deriving (Show, Eq, Ord, Generic)

data UtilCmd = UserCmd UserCmd | Deploy DeployOpts | Init InitOpts
  deriving (Show, Eq, Ord, Generic)

utilCmdP :: Opt.Parser UtilCmd
utilCmdP =
  Opt.hsubparser $
    Opt.command "user" (UserCmd <$> userCmdP)
      <> Opt.command "deploy" (Opt.info deployP $ Opt.progDesc "Deploy binaries & libraries from specified source")
      <> Opt.command "init" (Opt.info initP $ Opt.progDesc "Initialise GitOps Environment")
  where
    initP =
      Init <$> do
        daemonCfg <-
          optional $
            Opt.strOption $
              Opt.long "daemon-config"
                <> Opt.short 'c'
                <> Opt.help "The path to the daemon config"
                <> Opt.metavar "PATH"
        pure InitOpts {..}
    deployP =
      Deploy <$> do
        source <-
          Opt.strOption $
            Opt.long "input"
              <> Opt.short 'I'
              <> Opt.help "The path to the tarball for deployment"
              <> Opt.metavar "PATH"
        pure DeployOpts {..}

data UserCmd
  = RegisterUser RegisterUserOpts
  | UnregisterUser UnregisterUserOpts
  | IssueToken IssueOpts
  | RotateToken RotateOpts
  | ListUser
  deriving (Show, Eq, Ord, Generic)

newtype DeployOpts = DeployOpts {source :: FilePath}
  deriving (Show, Eq, Ord, Generic)

newtype InitOpts = InitOpts {daemonCfg :: Maybe FilePath}
  deriving (Show, Eq, Ord, Generic)

data IssueOpts = IssueOpts {userName :: Text, expiresInSecond :: Maybe Int}
  deriving (Show, Eq, Ord, Generic)

newtype RotateOpts = RotateOpts {userName :: Text}
  deriving (Show, Eq, Ord, Generic)

data RegisterUserOpts = RegisterUserOpts {userName :: Text, userPass :: Maybe Text, expiresInSecond :: Maybe Int}
  deriving (Show, Eq, Ord, Generic)

newtype UnregisterUserOpts = UnregisterUserOpts {userName :: Text}
  deriving (Show, Eq, Ord, Generic)

userCmdP :: Opt.ParserInfo UserCmd
userCmdP = Opt.info cmdsP $ Opt.progDesc "User-related commands"
  where
    cmdsP =
      Opt.hsubparser $
        Opt.command "register" (Opt.info registerP $ Opt.progDesc "Register a user")
          <> Opt.command "issue" (Opt.info issueP (Opt.progDesc "Issues Access Token for a user"))
          <> Opt.command "rotate" (Opt.info rotateP $ Opt.progDesc "Rotate a user token")
          <> Opt.command "unregister" (Opt.info unregisterP $ Opt.progDesc "Unregister a user")
          <> Opt.command "list" (Opt.info (pure ListUser) $ Opt.progDesc "List the registered users.")

    issueP =
      IssueToken <$> do
        userName <- userNameP
        expiresInSecond <- optional expirationP
        pure IssueOpts {..}
    rotateP =
      RotateToken <$> do
        userName <- userNameP
        pure RotateOpts {..}
    registerP =
      RegisterUser <$> do
        userName <- userNameP
        userPass <-
          optional $
            Opt.strOption $
              Opt.long "pass"
                <> Opt.short 'p'
                <> Opt.metavar "PASS"
                <> Opt.help "Optional password. Generate a random password when omitted."
        expiresInSecond <- optional expirationP
        pure RegisterUserOpts {..}
    unregisterP =
      UnregisterUser <$> do
        userName <- userNameP
        pure UnregisterUserOpts {..}

expirationP :: Opt.Parser Int
expirationP =
  Opt.option Opt.auto $
    Opt.long "expire"
      <> Opt.short 'E'
      <> Opt.metavar "SECOND"
      <> Opt.help "Expiration in second; when omitted, token is valid indefinitely."

userNameP :: Opt.Parser Text
userNameP = Opt.strOption $ Opt.long "user" <> Opt.short 'u' <> Opt.metavar "NAME" <> Opt.help "User name"
