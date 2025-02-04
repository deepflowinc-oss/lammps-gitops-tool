{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Development.LAMMPS.GitOps.Options (
  VersionInfo (..),
  Version,
  formatVersionInfo,
  VersionInfoQ,
  versionInfo,
  execOptionParserWithVersion,
) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Time (ZonedTime, getZonedTime)
import Data.Version (Version, showVersion)
import GHC.Generics (Generic)
import GitHash
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Options.Applicative

data VersionInfo = VersionInfo
  { progName :: !String
  , version :: !Version
  , builtOn :: !ZonedTime
  , gitInfo :: !GitInfo
  }
  deriving (Show, Generic)

type VersionInfoQ = CodeQ VersionInfo

versionInfo :: String -> Version -> VersionInfoQ
versionInfo prog v =
  [||
  VersionInfo
    { progName = $$(liftTyped prog)
    , version = $$(unsafeCodeCoerce $ liftData v)
    , builtOn = $$(unsafeCodeCoerce $ liftData =<< runIO getZonedTime)
    , gitInfo = $$tGitInfoCwd
    }
  ||]

execOptionParserWithVersion :: (MonadIO m) => VersionInfo -> ParserInfo opt -> m opt
execOptionParserWithVersion vinfo@VersionInfo {..} pinfo = liftIO $ do
  customExecParser (prefs subparserInline) $
    pinfo {infoParser = pinfo.infoParser <**> helper <**> versioner <**> numVersion}
  where
    verStr = showVersion version
    versioner =
      infoOption
        (formatVersionInfo vinfo)
        $ long "version" <> short 'V' <> help "Prints verbose version strings and exits"
    numVersion =
      infoOption verStr $
        long "numeric-version"
          <> help "Prints simple version number and exit"

formatVersionInfo :: VersionInfo -> String
formatVersionInfo VersionInfo {..} =
  progName <> ", " <> showVersion version <> " (commit: " <> giHash gitInfo <> ", built on: " <> show builtOn <> ")"
