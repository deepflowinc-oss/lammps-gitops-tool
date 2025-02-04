{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.LAMMPS.GitOps.App.Daemon.Model (
  migrateUserDb,
  UserDb (..),
  userDb,
  checkedUserDb,
  User,
  UserF (..),
  PrimaryKey (..),
  registerNewUser,
  rotateUserNonce,
  AuthUser (..),
  loginUser,
  UserAuthError (..),
  signAuthUser,
  verifyAuthUser,
) where

import Control.DeepSeq (NFData)
import Control.Exception.Safe
import Control.Lens (view, (^.))
import Crypto.JOSE (JWK)
import Crypto.JOSE qualified as JOSE
import Crypto.Nonce qualified as Nonce
import Crypto.PasswordStore (makePassword, verifyPassword)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.Function ((&))
import Data.Generics.Labels ()
import Data.Hashable (Hashable)
import Data.Reflection (Given)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Database.Beam
import Database.Beam.Migrate
import Database.Beam.Migrate.Simple (autoMigrate)
import Database.Beam.Query qualified as Beam
import Database.Beam.Sqlite
import Database.Beam.Sqlite.Migrate (migrationBackend)
import Database.SQLite3 (Error (..), SQLError (..))
import Development.LAMMPS.GitOps.Paths (EtcDir, getUserDbFile)
import Effectful (Eff, IOE, (:>))
import Effectful.Alias
import Effectful.Concurrent (Concurrent, runConcurrent)
import Effectful.Database.Beam.Sqlite hiding (Sqlite)
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.Fail
import Effectful.FileSystem (runFileSystem)
import Effectful.FileSystem.Tagged (createDirIfMissing)
import Effectful.Log.Extra
import Effectful.Random.Static (Random)
import Path.Tagged
import Servant.Auth.JWT (FromJWT, ToJWT)
import Servant.Auth.Server (defaultJWTSettings, makeJWT)

migrateUserDb ::
  (IOE ∈ es, Log ∈ es, Random ∈ es, Given (PathTo EtcDir Abs Dir)) =>
  Eff es ()
migrateUserDb = runConcurrent $ do
  dbPath <- liftIO getUserDbFile
  runFileSystem $ createDirIfMissing True $ parent dbPath
  runSqlite (DbFile $ untagPath dbPath) $ do
    either (throwM . userError) pure
      =<< runFail (Db.transactExclusive $ liftSqliteM $ autoMigrate migrationBackend checkedUserDb)

userDb :: DatabaseSettings Sqlite UserDb
userDb = unCheckDatabase checkedUserDb

checkedUserDb :: CheckedDatabaseSettings Sqlite UserDb
checkedUserDb = defaultMigratableDbSettings

newtype UserDb f = UserDb {users :: f (TableEntity UserF)}
  deriving (Generic)
  deriving anyclass (Database be)

data UserF f = User
  { name :: !(Columnar f Text)
  , pass :: !(Columnar f BS.ByteString)
  , nonce :: !(Columnar f Text)
  }
  deriving (Generic)
  deriving anyclass (Beamable)

data AuthUser = AuthUser {name :: !Text, nonce :: Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJWT, FromJWT)

loginUser ::
  (Random :> es, Log :> es, Concurrent :> es, Db.Sqlite :> es) =>
  T.Text ->
  T.Text ->
  Eff es (Either UserAuthError AuthUser)
loginUser name pass = do
  muser <-
    notransact $
      Db.selectFirst $
        Beam.lookup_ (userDb ^. #users) $
          UserKey name
  case muser of
    Nothing -> pure $ Left $ UnknownUser name
    Just usr@User {nonce}
      | verifyPassword (TE.encodeUtf8 pass) (usr ^. #pass) ->
          pure $ Right AuthUser {..}
      | otherwise -> pure $ Left InvalidPassword {..}

signAuthUser :: (MonadIO m) => JWK -> Maybe UTCTime -> AuthUser -> m (Either JOSE.Error LBS.ByteString)
signAuthUser jwk mexpire du = do
  liftIO $ makeJWT du (defaultJWTSettings jwk) mexpire

data UserAuthError
  = UnknownUser {name :: !T.Text}
  | NonceExpired {name :: !T.Text}
  | InvalidPassword {name :: !T.Text}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (Exception)

rotateUserNonce ::
  (Random :> es, Log :> es, Concurrent :> es, Db.Sqlite :> es, IOE :> es) =>
  T.Text ->
  Eff es AuthUser
rotateUserNonce uname = do
  let UserDb {users} = userDb
  nonce' <- liftIO $ Nonce.withGenerator Nonce.nonce128urlT
  transactExclusive $ do
    Db.update_ $
      Beam.update
        users
        (\u -> u ^. #nonce <-. val_ nonce')
        (\u -> u ^. #name ==. val_ uname)
    User {..} <-
      maybe (error $ "User not found: " <> T.unpack uname) pure
        =<< Db.selectFirst (Beam.lookup_ users (UserKey uname))
    pure AuthUser {..}

verifyAuthUser ::
  (Random :> es, Log :> es, Concurrent :> es, Db.Sqlite :> es) =>
  AuthUser ->
  Eff es (Either UserAuthError AuthUser)
verifyAuthUser au@AuthUser {..} = do
  muser <-
    notransact $
      Db.selectFirst $
        Beam.lookup_ (userDb ^. #users) $
          UserKey name
  case muser of
    Nothing -> pure $ Left $ UnknownUser name
    Just trueUser
      | nonce == trueUser ^. #nonce -> pure $ Right au
      | otherwise -> pure $ Left $ NonceExpired name

registerNewUser ::
  (Random :> es, Log :> es, Concurrent :> es, Db.Sqlite :> es, IOE :> es) =>
  User ->
  Eff es AuthUser
registerNewUser user = do
  user' <- user & #pass (liftIO . flip makePassword 17)
  mans <-
    try $
      transactExclusive $
        Db.insert_ (userDb ^. #users) $
          Db.insertValues [user']
  case mans of
    Left exc@SQLError {sqlError = ErrorConstraint} -> do
      logAttention_ "User creation failed; Perhaps user is already registered?"
      throwM exc
    Left sqlError -> do
      logAttention_ "Unexpected sql exception had occurred!"
      throwM sqlError
    Right () -> pure AuthUser {name = user ^. #name, nonce = user ^. #nonce}

type User = UserF Identity

deriving instance Show User

deriving instance Eq User

deriving instance Ord User

deriving instance Hashable User

deriving instance NFData User

instance Table UserF where
  newtype PrimaryKey UserF f = UserKey (Columnar f Text)
    deriving (Generic)
    deriving anyclass (Beamable)
  primaryKey = UserKey . view #name
