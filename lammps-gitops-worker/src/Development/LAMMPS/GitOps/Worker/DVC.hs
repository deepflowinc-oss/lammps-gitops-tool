{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
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

module Development.LAMMPS.GitOps.Worker.DVC (
  isDvcEnabled,
  setupDvc,
  dvcRepro,
  finaliseDvcRepro,
  getDvcParams,
  DvcYaml (..),
  DvcStage (..),
  DvcParamDef (..),
  dvc_,
) where

import Control.Applicative ((<|>))
import Control.Exception.Safe (Exception (..), catchAny)
import Control.Lens (ifoldMap)
import Control.Lens qualified as Lens
import Control.Lens.Extras (is)
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.Key qualified as AK
import Data.Aeson.KeyMap qualified as AKM
import Data.ByteString.Lazy qualified as LBS
import Data.ByteString.Lazy.Char8 qualified as LBS8
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Functor.Compose (Compose (..))
import Data.HashSet qualified as HS
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust, mapMaybe)
import Data.Monoid (Alt (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Yaml qualified as Y
import Development.LAMMPS.GitOps.Paths.Workflow
import Development.LAMMPS.GitOps.Types
import Development.LAMMPS.GitOps.Worker.Toolchain
import Development.LAMMPS.GitOps.Worker.Types
import Effectful
import Effectful.Concurrent (Concurrent)
import Effectful.Concurrent.TimedResource (Expiration)
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.FileSystem (FileSystem)
import Effectful.FileSystem.Tagged (createDirIfMissing, doesFileExist, readFileBinaryLazy, setFileMode, withCurrentDir, writeTextStrict)
import Effectful.Log.Extra
import Effectful.Network.GitHub.Apps (GitTree (..), NewCommit (..), createCommit, createTree, fromBlobPaths, getCommitObj, updateRefs)
import Effectful.Network.GitHub.Apps qualified as GitHub
import Effectful.Network.Http (runSimpleHttp)
import Effectful.Process.Typed
import Effectful.Random.Static
import Effectful.Reader.Static
import Effectful.Reader.Static.Lens qualified as EffL
import GHC.Exts qualified as GHC
import GHC.Generics (Generic)
import NeatInterpolation (trimming)
import Network.S3.Sign.V4
import Path.Tagged
import Text.Show.Extra (tshow)

isDvcEnabled :: (FileSystem :> es, Reader WorkerEnv :> es) => Eff es Bool
isDvcEnabled = do
  isDvcJob <- EffL.view $ #workflow . #job . Lens.to (is #_DVCRepro)
  dvcYamlThere <- doesFileExist . (</> [relfile|dvc.yaml|]) =<< viewClonedDir
  pure $ isDvcJob || dvcYamlThere

setupDvc ::
  (FileSystem :> es, Log :> es, Reader WorkerEnv :> es, TypedProcess :> es) =>
  Eff es ()
setupDvc = do
  dir <- viewClonedDir
  localDomain "setup-dvc" $ withCurrentDir dir $ do
    createDirIfMissing True dir
    let dvcConfig = dir </> [relfile|.dvc/config.local|]
        remote = "storage"
    storageSect <- toStorageSection remote <$> EffL.view (#repoConfig . #storage)

    createDirIfMissing True $ parent dvcConfig
    setFileMode (parent dvcConfig) 0o700
    writeTextStrict dvcConfig ""
    setFileMode dvcConfig 0o600
    writeTextStrict
      dvcConfig
      [trimming|
        [core]
          remote = ${remote}
          analytics = false
          autostage = true
        ${storageSect}
      |]
    dvc_ dir ["pull", "-A", "-f", "-j", "8"] `catchAny` \e ->
      logAttention_ $ "dvc pull failed - perhaps empty repository?: " <> T.pack (displayException e)
    dvc_ dir ["exp", "pull", "origin", "-A", "-f", "-j", "8"] `catchAny` \e ->
      logAttention_ $ "dvc exp pull failed - perhaps empty repository?: " <> T.pack (displayException e)
    pure ()

toStorageSection :: Text -> StorageConfig -> Text
toStorageSection remoteName S3Storage {..} =
  let keyId = T.decodeUtf8 s3.keys.accessKeyId.getUtf8
      keySecret = T.decodeUtf8 s3.keys.accessKeySecret.getUtf8
      endpoint = tshow s3.s3Endpoint
      bucketPath = maybe bucket ((bucket <> "/") <>) bucketSubdir
   in [trimming|
        ['remote "${remoteName}"']
          url = s3://${bucketPath}
          access_key_id = ${keyId}
          secret_access_key = ${keySecret}
          endpointurl = ${endpoint}
      |]
toStorageSection remoteName (LocalStorage fp) =
  let url = T.pack $ fromAbsDir fp
   in [trimming|
        ['remote "${remoteName}"']
          url = ${url}
      |]

dvcRepro ::
  (Log :> es, Reader WorkerEnv :> es, TypedProcess :> es) =>
  Maybe (NonEmpty Text) ->
  Eff es ()
dvcRepro mtargets = do
  workdir <- viewClonedDir
  dvc_ workdir $ "repro" : maybe [] (map T.unpack . NE.toList) mtargets

-- | Make a commit and push to the original branch
finaliseDvcRepro ::
  ( Log :> es
  , Reader WorkerEnv :> es
  , TypedProcess :> es
  , FileSystem :> es
  , Expiration :> es
  , Random :> es
  , Concurrent :> es
  , IOE :> es
  ) =>
  Eff es ()
finaliseDvcRepro = localDomain "dvc-finalise" $ do
  workdir <- viewClonedDir
  diffs <-
    mapMaybe (parseRelFile @GHC.Any @JobCloneDir . LT.unpack . LT.decodeUtf8)
      . filter (not . LBS8.null)
      . LBS8.lines
      <$> readProcessStdout_
        ( proc "git" ["diff", "--cached", "--name-only"]
            & setWorkingDir (fromAbsDir workdir)
        )
  if null diffs
    then logInfo_ "No updates detected. Skipping making commit"
    else do
      logInfo_ "executing dvc push..."
      dvc_ workdir ["push"]
      logInfo_ "dvc pushed."
      comm <- EffL.view #commit
      repoID <- EffL.view #repo
      FullContext {..} <-
        fromJust
          <$> withGitHubDb
            ( Db.notransact $
                Db.selectFirst $
                  selectFullContext repoID comm
            )
      runSimpleHttp $ runRepoAPI $ do
        let shortComm = comm.getCommitHash
            prNum = tshow pull.pullNumber
            message = [trimming|[gitops] Experiment result(s) for ${shortComm} (PR #${prNum})|]

        entries <- fromBlobPaths workdir diffs
        logInfo "Updated file(s)" entries
        origComm <- getCommitObj $ coerce comm
        logTrace "Original commit" $ tshow origComm
        tree <-
          createTree
            GitTree
              { base_tree = Just origComm.tree.sha
              , tree = entries
              }
        logTrace "Tree created" tree
        newComm <-
          createCommit
            NewCommit
              { parents = [origComm.sha]
              , tree = tree.sha
              , message
              }
        logTrace "Commit created" $ tshow newComm
        let br = pull.branch
        ref <- updateRefs ("heads/" <> br) newComm.sha
        logTrace [trimming|Updated branch ${br}|] ref

dvc_ ::
  (TypedProcess :> es, Reader WorkerEnv :> es, Log :> es) =>
  PathTo e Abs Dir ->
  [String] ->
  Eff es ()
dvc_ root args = do
  tc <- EffL.view #toolchain
  runReader tc $
    runProcess_ . setWorkingDir (fromAbsDir root)
      =<< procToolchain "dvc" args

data DvcStage = DvcStage
  { cmd :: !Text
  , wdir :: Maybe FilePath
  , deps :: Maybe [FilePath]
  , params :: Maybe [DvcParamDef]
  , outs :: Maybe [FilePath]
  }
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON)

data DvcParamDef
  = DefaultParam Text
  | FromYamlFile FilePath (Maybe [Text])
  deriving (Show, Eq, Ord, Generic)

instance FromJSON DvcParamDef where
  parseJSON =
    runReaderT $
      getAlt $
        Alt (ReaderT $ J.withText "<parameter>" (pure . DefaultParam))
          <|> Alt
            ( ReaderT $
                J.withObject "paramfile: {...}" \obj ->
                  case AKM.toList obj of
                    [(fp, j)] ->
                      case j of
                        J.Null -> pure $ FromYamlFile (AK.toString fp) Nothing
                        _ -> FromYamlFile (AK.toString fp) . Just <$> J.parseJSON j
                    ents ->
                      fail $
                        "Given dic is empty or has more than two keys: "
                          <> show (map fst ents)
            )

instance ToJSON DvcParamDef

data DvcYaml = DvcYaml
  { params :: Maybe [DvcParamDef]
  , stages :: Maybe (AKM.KeyMap DvcStage)
  }
  deriving (Show, Eq, Ord, Generic)
  deriving (FromJSON, ToJSON)

getParamFilesFor :: Maybe (NonEmpty Text) -> DvcYaml -> [PathTo ReportResource (RelTo JobCloneDir) File]
getParamFilesFor mstages dvc =
  Set.toList $
    foldMap (extractWith Nothing) (Compose dvc.params)
      <> foldMap
        ( ifoldMap
            ( \stage DvcStage {..} ->
                if maybe True (HS.member $ AK.toText stage) targStages
                  then foldMap (extractWith wdir) $ Compose params
                  else mempty
            )
        )
        dvc.stages
  where
    targStages = HS.fromList . NE.toList <$> mstages
    extractWith mwdir x =
      let withPrfx = maybe id ((</>) . fromJust . parseRelDir) mwdir
       in Set.singleton $ withPrfx $ case x of
            DefaultParam {} -> [relfile|params.yaml|]
            FromYamlFile fp _ -> fromJust $ parseRelFile fp

getDvcParams ::
  (FileSystem :> es, IOE :> es) =>
  PathTo JobCloneDir Abs Dir ->
  Maybe (NonEmpty Text) ->
  Eff es (Map (PathTo ReportResource (RelTo JobCloneDir) File) LBS.ByteString)
getDvcParams cloned mtargets = do
  dvc <- Y.decodeFileThrow $ fromAbsFile $ cloned </> [relfile|dvc.yaml|]
  let files = getParamFilesFor mtargets dvc
  Map.fromList <$> mapM (\fp -> (fp,) <$> readFileBinaryLazy (cloned </> fp)) files
