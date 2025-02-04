{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults -Wno-orphans #-}

module Main (main) where

import Control.Applicative (optional, (<**>))
import Control.Concurrent
import Control.Exception (Exception, throwIO, try)
import Control.Monad (forM_)
import Control.Monad.Fix (fix)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text.Encoding as T
import Data.Time (getCurrentTime)
import Data.Time.Format
import qualified Data.Yaml as Y
import Development.Job.DRMAA.V2.HighLevel
import Development.Job.DRMAA.V2.HighLevel.JobSession
import Development.Job.DRMAA.V2.LowLevel.Error (DRMAAException)
import Development.Job.DRMAA.V2.LowLevel.ImplementationSpecific
import Development.Job.DRMAA.V2.LowLevel.Types (OpaqueExt)
import Foreign.C (CInt)
import GHC.Generics (Generic)
import qualified Options.Applicative as Opts
import RIO (async, race, wait)
import qualified RIO.HashMap as HM
import System.Directory (getCurrentDirectory)
import System.FilePath

instance A.ToJSON CInt where
  toJSON = A.toJSON . toInteger

instance A.FromJSON CInt where
  parseJSON p = fromInteger <$> A.parseJSON p

deriving newtype instance A.ToJSON OS

deriving newtype instance A.FromJSON OS

deriving newtype instance A.ToJSON CPU

deriving newtype instance A.FromJSON CPU

instance A.ToJSON OpaqueExt where
  toJSON = pure A.Null

instance A.FromJSON OpaqueExt where
  parseJSON = const $ fail "OpaqueExt"

instance A.FromJSON BS.ByteString where
  parseJSON = A.withText "text" $ pure . T.encodeUtf8

instance A.ToJSON BS.ByteString where
  toJSON = A.toJSON . T.decodeUtf8

instance A.FromJSONKey BS.ByteString where
  fromJSONKey = A.FromJSONKeyText T.encodeUtf8

instance A.ToJSONKey BS.ByteString

data AppOpt = AppOpt {config :: Maybe FilePath, inspectJob :: Maybe BS.ByteString}
  deriving (Show, Eq, Ord)

appOptP :: Opts.ParserInfo AppOpt
appOptP =
  Opts.info (p <**> Opts.helper) $
    Opts.progDesc "DRMAAv2 client with job scheduling"
  where
    p = do
      config <-
        optional
          ( Opts.strOption $
              Opts.long "input"
                <> Opts.short 'i'
                <> Opts.metavar "YAML"
                <> Opts.help "Path to the yaml"
          )
      inspectJob <-
        optional
          ( Opts.strOption $
              Opts.long "inspect-job"
                <> Opts.short 'J'
                <> Opts.metavar "JOBID"
                <> Opts.help "Optional Job ID to inspect before scheduling"
          )

      pure AppOpt {..}

data JobTransitionException
  = FailedToStart DRMAAException
  | JobAborted DRMAAException
  | JobSeemDeleted JobState BS.ByteString
  deriving (Show, Generic)
  deriving anyclass (Exception)

main :: IO ()
main = do
  let aJob = "test_session_new"
  AppOpt {..} <- Opts.execParser appOptP
  cwd <- getCurrentDirectory
  stamp <- formatTime defaultTimeLocale "%Y%m%d-%H:%M:%S" <$> getCurrentTime
  tmplt <- case config of
    Just p -> Y.decodeFileThrow p
    Nothing ->
      pure
        (mempty @JobTemplate)
          { payload =
              mempty
                { remoteCommand = Just "bash"
                , args = Just ["-c", "echo Hello; mkdir -p workspace; sleep 30; echo OK: " <> BS8.pack stamp <> " > workspace/test.txt"]
                , jobName = Just "test_job"
                , workingDirectory = Just $ BS8.pack cwd
                , outputPath = Just $ BS8.pack $ cwd </> "test_job" <.> stamp <.> "o"
                , -- , inputPath = Just "Nothing here"
                  errorPath = Nothing
                }
          }
  withJobSession defaultSessionOption {closeFinally = False} aJob $ \js -> do
    cats <- getJobCategories js
    putStrLn $ "Available Job Categories: " <> show cats
    putStrLn $ "Job Template extension(s): " <> show jobTemplateImplSpec
    forM_ inspectJob $ \jid0 -> do
      jobs <- getJobs js (Just (mempty @JobFilter) {payload = mempty {jobId = Just jid0}})
      putStrLn $ "# of matching jobs: " <> show jobs
      jss <- mapM getJobState jobs
      putStrLn $ "Job States: " <> show jss
      jis <- mapM getJobInfo jobs
      putStrLn $ "Job Infos: " <> show jis
      jis' <- mapM getJobInfo jobs
      putStrLn $ "Job Infos: " <> show jis'
    sessName <- getSessionName js
    putStrLn $ "Session Opened: " <> BS8.unpack sessName
    putStrLn $ "Schduling job for: " <> show tmplt
    j <- runJob js tmplt
    jid <- getJobID j
    putStrLn . ("getJobID: " <>) . show =<< getJobID j
    putStr "JobArrayJobs: "
    print =<< mapM getJobInfo =<< getJobArrayJobs =<< getJobArray js jid
    putStrLn $ "ID from RawInfo: " <> BS8.unpack jid
    eWait <- async $ try $ do
      putStrLn "Waiting for start..."
      either (throwIO . FailedToStart) pure
        =<< try (waitStarted j Nothing)
      putStrLn "Job Started"
      putStrLn "Waiting for finish..."
      either (throwIO . JobAborted) pure =<< try (waitTerminated j Nothing)
      putStrLn "Job Finished"
      getJobState j
    stateMonitor <- async $ try $ fix $ \self -> do
      -- FIXME: Bump up if debugging is over
      threadDelay $ 30 * 10 ^ (6 :: Int)
      jinfo <- getJobInfo j
      st <- getJobState j
      if
          | st >= Done -> pure st
          | st < Running
          , Just failCode <- HM.lookup "uge_ji_failed" =<< implSpec =<< jinfo
          , failCode /= "-" ->
              throwIO $ JobSeemDeleted st failCode
          | otherwise -> self

    e <- either id id <$> wait eWait `race` wait stateMonitor
    putStrLn . ("Final info:" <>) . show =<< getJobInfo j
    case e of
      Right st -> do
        putStrLn $ "Job finished with code: " <> show st
      Left (FailedToStart err) ->
        putStrLn $ "Job failed to start: " <> show err
      Left (JobAborted err) ->
        putStrLn $ "Job aborted: " <> show err
      Left (JobSeemDeleted code f) ->
        putStrLn $ "Job deleted: " <> show (code, f)
