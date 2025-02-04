{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module Development.LAMMPS.GitOps.Webhook.SlashCommand (
  parseSlashCommand,
  SlashCommand (..),
  CommandParseResult (..),
  toCommandParseError,
) where

import Control.Monad (when)
import Data.CaseInsensitive qualified as CI
import Data.Char qualified as C
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Data.Text (Text)
import Data.Text qualified as T
import Development.LAMMPS.GitOps.Types (CommitHash (..))
import GHC.Generics (Generic)

data SlashCommand
  = Cancel !(Maybe (NonEmpty CommitHash))
  | CancelAll
  | Run !(Maybe (NonEmpty CommitHash))
  deriving (Show, Eq, Ord, Generic)

data CommandParseResult
  = UnknownCommand {command :: T.Text, message :: T.Text}
  | InvalidCommand {command :: T.Text, message :: T.Text}
  | NotACommand
  deriving (Show, Eq, Ord, Generic)

toCommandParseError :: CommandParseResult -> Maybe Text
toCommandParseError NotACommand = Nothing
toCommandParseError UnknownCommand {..} =
  Just $ "Command parse error: " <> message <> " (Input: " <> T.pack (show command) <> ")"
toCommandParseError InvalidCommand {..} =
  Just $ "Command parse error: " <> message <> " (Input: " <> T.pack (show command) <> ")"

parseSlashCommand :: Text -> Either CommandParseResult SlashCommand
parseSlashCommand (T.strip -> raw) = do
  (r, rest) <- maybe (Left NotACommand) Right $ T.uncons raw
  when (r /= '/') $
    Left NotACommand
  let (cmd, T.stripStart -> margs) = T.break C.isSpace rest
      args = map CI.mk $ T.words margs
  when (T.null cmd) $
    Left InvalidCommand {command = raw, message = "Empty command"}
  case CI.mk cmd of
    "cancel"
      | "all" `elem` args -> pure CancelAll
      | otherwise -> pure $ Cancel $ nonEmpty $ map CommitHash $ T.words margs
    "run" -> pure $ Run $ nonEmpty $ map CommitHash $ T.words margs
    _ -> Left UnknownCommand {command = raw, message = "Unknown command `" <> cmd <> "'"}

-- >>> parseSlashCommand "/cancel all"
-- Right CancelAll
