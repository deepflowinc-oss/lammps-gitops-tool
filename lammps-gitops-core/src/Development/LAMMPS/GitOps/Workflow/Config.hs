{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.LAMMPS.GitOps.Workflow.Config (
  Workflow (..),
  Resources (..),
  Script (..),
  JobScript (..),
  Toolchain (..),
  SchedulerType (..),
) where

import Control.Monad (unless, (>=>))
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as J
import Data.Aeson.KeyMap qualified as AKM
import Data.Aeson.Types qualified as AT
import Data.Char qualified as C
import Data.HashMap.Strict (HashMap)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (fromMaybe)
import Data.Monoid (Alt (..))
import Data.Semigroup.Foldable (foldMap1)
import Data.Text (Text)
import Data.Text qualified as T
import Development.Job.Scheduler.Class (Duration)
import Development.LAMMPS.GitOps.Paths (JobCloneDir, ReportResourceDir)
import GHC.Generics
import Path.Tagged (Dir, PathTo, RelTo)

data SchedulerType = UgeDrmaa2
  deriving (Show, Eq, Ord, Generic)

schedOpts :: J.Options
schedOpts =
  J.defaultOptions
    { J.constructorTagModifier = J.camelTo2 '_'
    , J.sumEncoding = J.UntaggedValue
    , J.tagSingleConstructors = True
    }

instance J.ToJSON SchedulerType where
  toJSON = J.genericToJSON schedOpts

instance J.FromJSON SchedulerType where
  parseJSON = J.genericParseJSON schedOpts

data Workflow = Workflow
  { setup :: Maybe Script
  , job :: JobScript
  , queue :: Maybe Text
  , cleanup :: Maybe Script
  , modules :: Maybe [Text]
  , resources :: Resources
  , priority :: Maybe Int
  , postDirs :: [PathTo ReportResourceDir (RelTo JobCloneDir) Dir]
  , toolchain :: Maybe Toolchain
  }
  deriving (Show, Eq, Ord, Generic)

workflowOpts :: J.Options
workflowOpts =
  J.defaultOptions
    { J.rejectUnknownFields = True
    , J.omitNothingFields = True
    }

instance J.FromJSON Workflow where
  parseJSON = J.genericParseJSON workflowOpts

instance J.ToJSON Workflow where
  toJSON :: Workflow -> AT.Value
  toJSON = J.genericToJSON workflowOpts

data Resources = Resources
  { nodes :: !(Maybe Word)
  , coresPerNode :: !Word
  , duration :: !(Maybe Duration)
  , gpus :: !(Maybe Word)
  , others :: Maybe (HashMap Text Text)
  }
  deriving (Show, Eq, Ord, Generic)

resourcesOpts :: J.Options
resourcesOpts =
  J.defaultOptions
    { J.omitNothingFields = True
    , J.rejectUnknownFields = True
    }

instance ToJSON Resources where
  toJSON = J.genericToJSON resourcesOpts

instance FromJSON Resources where
  parseJSON =
    J.genericParseJSON resourcesOpts >=> \opts ->
      opts <$ do
        unless (fromMaybe 1 opts.nodes * opts.coresPerNode > 0)
          $ fail
          $ "Number of threads must be > 0, but got: (nodes, coresPerNodes) = "
          <> show (fromMaybe 1 opts.nodes, opts.coresPerNode)

data Script = Exec !Text | Path !FilePath
  deriving (Show, Eq, Ord, Generic)

scrOpts :: J.Options
scrOpts =
  J.defaultOptions
    { J.allNullaryToStringTag = True
    , J.constructorTagModifier = map C.toLower
    , J.sumEncoding = J.ObjectWithSingleField
    , J.rejectUnknownFields = True
    }

instance ToJSON Script where
  toJSON = J.genericToJSON scrOpts

instance FromJSON Script where
  parseJSON = J.genericParseJSON scrOpts

data JobScript
  = Script Script
  | DVCRepro (Maybe (NonEmpty Text))
  deriving (Show, Eq, Ord, Generic)

instance J.FromJSON JobScript where
  parseJSON =
    runReaderT
      $ getAlt
      $ foldMap1
        (Alt . ReaderT)
      $ (fmap Script <$> J.parseJSON)
      :| [ J.withText "dvc-repro" $ \txt -> do
            "dvc-repro" <- pure $ T.toLower txt
            pure $ DVCRepro Nothing
         , J.withObject "{dvc-repro: [..]}" $ \dic ->
            case AKM.toList dic of
              [("dvc-repro", ls)] -> DVCRepro <$> J.parseJSON ls
              [] -> fail "empty dictionary"
              keys -> fail $ "Object with `dvc-repro' as the only key expected, but got: " <> show (map fst keys)
         ]

instance J.ToJSON JobScript where
  toJSON (Script path) = J.toJSON path
  toJSON (DVCRepro Nothing) = "dvc-repro"
  toJSON (DVCRepro (Just ls)) = J.object ["dvc-repro" J..= ls]

data Toolchain = Global | Rye | Poetry
  deriving (Show, Eq, Ord, Generic)

toolOpts :: AT.Options
toolOpts = J.defaultOptions {J.constructorTagModifier = map C.toLower}

instance J.FromJSON Toolchain where
  parseJSON = J.genericParseJSON toolOpts

instance J.ToJSON Toolchain where
  toJSON = J.genericToJSON toolOpts
