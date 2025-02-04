{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.Worker.Toolchain (detectToolchain, initToolchain, procToolchain) where

import Control.Monad (guard)
import Data.Function ((&))
import Data.Generics.Labels ()
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Development.LAMMPS.GitOps.Paths
import Development.LAMMPS.GitOps.Workflow.Config
import Effectful
import Effectful.Environment hiding (setEnv)
import Effectful.FileSystem.Tagged
import Effectful.Log.Extra
import Effectful.NonDet
import Effectful.Process.Typed
import Effectful.Process.Typed.Log
import Effectful.Reader.Static (Reader, ask)
import Path.Tagged
import Text.Show.Extra (tshow)
import Toml (Table' (..), Value' (..))
import qualified Toml

procToolchain ::
  ( Log :> es
  , Reader Toolchain :> es
  ) =>
  FilePath ->
  [String] ->
  Eff es (ProcessConfig () () ())
procToolchain prog args = do
  tc <- ask
  delegateOutputAndErrorToLog $ case tc of
    Global -> proc prog args
    Rye -> proc "rye" $ "run" : prog : args
    Poetry -> proc "poetry" $ "run" : prog : args

initToolchain ::
  (TypedProcess :> es, Log :> es, Environment :> es) =>
  Toolchain ->
  PathTo JobCloneDir Abs Dir ->
  Eff es ()
initToolchain backend dest = localDomain "initialise" $ do
  logInfo_ $ "Initialising " <> T.pack (fromAbsDir dest) <> " with " <> tshow backend <> "."
  case backend of
    Rye -> do
      let pc =
            proc "rye" ["sync", "--no-lock"]
              & setWorkingDir (fromAbsDir dest)
      runProcessLogged_ "rye-sync" pc
    Poetry -> do
      env <- getEnvironment
      let pc =
            proc "poetry" ["install"]
              & setWorkingDir (fromAbsDir dest)
              & setEnv
                ( Map.toList
                    $ Map.insert
                      "PYTHON_KEYRING_BACKEND"
                      "keyring.backends.null.Keyring"
                    $ Map.fromList env
                )
      runProcessLogged_ "poetry-install" pc
    Global -> logInfo_ "Nothing to do with Global backend."
  logInfo_ "Initialisation done!"

detectToolchain ::
  (FileSystem :> es, Log :> es) =>
  PathTo JobCloneDir Abs Dir ->
  Eff es (Maybe Toolchain)
detectToolchain cloned = localDomain "detectToolchain" $ do
  tc <- fmap (either (const Nothing) Just) $
    runNonDet OnEmptyKeep $ do
      logInfo_ "Detecting tool chain..."
      let pyproj = cloned </> [relfile|pyproject.toml|]
      thereProject <- doesFileExist pyproj

      if thereProject
        then do
          logInfo_ "pyproject.toml found."
          reqLockThere <- doesFileExist $ cloned </> [relfile|requirements.lock|]
          poetryLockThere <- doesFileExist $ cloned </> [relfile|poetry.lock|]
          guard thereProject
          MkTable toml <-
            either (const empty) pure . Toml.parse . T.decodeUtf8
              =<< readFileBinaryStrict pyproj
          let mtool = snd <$> Map.lookup "tool" toml
              ryeToolThere = fromMaybe False do
                Table' _ (MkTable dic) <- mtool
                pure $ Map.member "rye" dic
              poetryToolThere = fromMaybe False do
                Table' _ (MkTable dic) <- mtool
                pure $ Map.member "poetry" dic
          if
            | reqLockThere && not poetryLockThere ->
                Rye <$ logInfo_ "Only requirements.lock found."
            | poetryLockThere ->
                Poetry <$ logInfo_ "Only poetry.lock found."
            | ryeToolThere && not poetryToolThere ->
                Rye <$ logInfo_ "No files found; pyproject.toml has only tool.rye"
            | poetryToolThere ->
                Poetry
                  <$ logInfo_ "No files found; pyproject.toml has only tool.oietrt"
            | otherwise -> empty
        else do
          logInfo_ "No pyproject.toml found. Could not determine."
          empty
  logInfo_ $ "Detected toolchain: " <> tshow tc
  pure tc