{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

module Development.LAMMPS.GitOps.Deployment (
  deployFrom,
  SomeDepPath (..),
  untagSomeDep,
  Deployment (..),
  defaultDeployment,
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (onException)
import Control.Lens ((^?))
import Control.Monad (when)
import Data.Function (on, (&))
import Data.Functor (void)
import Data.Generics.Labels ()
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.Maybe (fromMaybe)
import Data.Reflection (Given)
import Data.Text qualified as T
import Development.LAMMPS.GitOps.Paths
import Effectful
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem qualified as EffFS
import Effectful.FileSystem.Tagged (createDirIfMissing, doesFileExist, setFileMode)
import Effectful.FileSystem.Streaming qualified as QE
import Effectful.Log.Extra
import GHC.Generics (Generic)
import Numeric (showOct)
import Path.Tagged
import Path.Tagged qualified as TP
import Streaming.Archive.Tar qualified as QT
import Streaming.ByteString qualified as Q
import Streaming.Zip qualified as QZ
import System.FilePath qualified as RawFP
import System.Posix.Types (FileMode)
import Text.Show.Extra (tshow)

data SomeDepPath d where
  MkSomeDep :: PathTo p (RelTo d) File -> SomeDepPath d

untagSomeDep :: SomeDepPath d -> FilePath
untagSomeDep (MkSomeDep bp) = fromRelFile bp

instance Show (SomeDepPath d) where
  showsPrec d = showsPrec d . untagSomeDep

instance Eq (SomeDepPath d) where
  (==) = (==) `on` untagSomeDep

instance Hashable (SomeDepPath d) where
  hashWithSalt s = hashWithSalt s . untagSomeDep

data Deployment = Deployment
  { libDestDir :: PathTo LibDir Abs Dir
  , libs :: HashMap (SomeDepPath LibDir) FileMode
  , binDestDir :: PathTo BinDir Abs Dir
  , bins :: HashMap (SomeDepPath BinDir) FileMode
  }
  deriving (Show, Eq, Generic)

defaultDeployment :: (Given (PathTo EtcDir Abs Dir)) => IO Deployment
defaultDeployment = do
  libDestDir <- getAppLibDir
  binDestDir <- getAppBinDir
  pure
    Deployment
      { libs = HM.fromList [(MkSomeDep daemonObj, 0o760), (MkSomeDep workerObj, 0o760)]
      , bins =
          HM.fromList
            [ (MkSomeDep daemonBin, 0o760)
            , (MkSomeDep clientBin, 0o775)
            , (MkSomeDep adminBin, 0o770)
            , (MkSomeDep workerBin, 0o760)
            ]
      , ..
      }

-- FIXME: バイナリやライブラリが足りなかったら警告
deployFrom ::
  (FileSystem :> es, IOE :> es, Log :> es) =>
  Deployment ->
  Q.ByteStream (Eff es) () ->
  Eff es ()
deployFrom Deployment {..} _stream = do
  void $
    _stream
      & QZ.gunzip
      & QT.untarWith \e@QT.Entry {..} body ->
        case content ^? #_File of
          Nothing -> do
            logInfo_ $ "Skipping: " <> tshow e
            Q.effects body
          Just {} -> do
            let mkDeployer ::
                  T.Text ->
                  TP.PathTo d TP.Abs TP.Dir ->
                  TP.PathTo d (TP.RelTo DataDir) TP.Dir ->
                  HashMap (SomeDepPath d) _ ->
                  Maybe _
                mkDeployer kind depAbs p dic = do
                  targP <-
                    TP.stripProperPrefix p
                      =<< TP.parseRelFile (RawFP.joinPath $ drop 1 $ RawFP.splitPath entryPath)
                  perm <- HM.lookup (MkSomeDep targP) dic
                  pure $ do
                    let dest = depAbs </> targP
                    logInfo_ $
                      "Deploying "
                        <> kind
                        <> ": "
                        <> T.pack (TP.fromRelFile targP)
                        <> " to "
                        <> T.pack (TP.fromAbsFile dest)
                        <> " (mode: "
                        <> T.pack (showOct perm "")
                        <> ")"
                    createDirIfMissing True $ TP.parent dest
                    there <- doesFileExist dest
                    bakFile <- addExtension ".bak" dest
                    when there $
                      EffFS.renameFile (fromAbsFile dest) (fromAbsFile bakFile)
                    ( (QE.writeFile dest body <* setFileMode dest perm)
                        `onException` EffFS.renameFile (fromAbsFile bakFile) (fromAbsFile dest)
                      )
                      <* do
                        bakRemain <- doesFileExist bakFile
                        when bakRemain $ EffFS.removeFile $ fromAbsFile bakFile
            fromMaybe (logInfo_ ("Skipping: " <> tshow e) >> Q.effects body) $
              mkDeployer "Lib" libDestDir libDir libs
                <|> mkDeployer "Bin" binDestDir binDir bins