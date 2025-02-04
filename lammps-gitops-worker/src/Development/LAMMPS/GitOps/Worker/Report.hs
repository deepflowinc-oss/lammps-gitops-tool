{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -fplugin Effectful.Plugin #-}

module Development.LAMMPS.GitOps.Worker.Report (
  uploadReport,
  buildReportHtml,
  generateReportSeed,
  s3JobReportDir,
  updateReportIndex,
) where

import CMark qualified
import Control.Arrow ((>>>))
import Control.Exception.Safe hiding (throwString)
import Control.Lens (Identity (runIdentity), iforM_, (%~), _2)
import Control.Lens.At qualified as Lens
import Control.Lens.Getter qualified as Lens
import Control.Monad (forM, forM_, unless, when)
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Lazy qualified as LBS
import Data.Char qualified as C
import Data.DList qualified as DL
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Reflection (Given)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import Data.Text.Lazy qualified as LT
import Data.Text.Lazy.Encoding qualified as LT
import Data.Text.Lens qualified as TL
import Data.Time (defaultTimeLocale, formatTime, getCurrentTimeZone, utcToZonedTime)
import Data.Time.LocalTime (TimeZone)
import Database.Beam (desc_, (&&.), (==.))
import Database.Beam qualified as Beam
import Database.Beam.Query (orderBy_, val_)
import Development.LAMMPS.GitOps.Paths.Workflow
import Development.LAMMPS.GitOps.Types
import Development.LAMMPS.GitOps.Worker.DVC
import Development.LAMMPS.GitOps.Worker.Types
import Development.LAMMPS.GitOps.Workflow.Config
import Effectful
import Effectful.Concurrent
import Effectful.Database.Beam.Sqlite qualified as Db
import Effectful.Exception (throwString)
import Effectful.FileSystem (FileSystem, runFileSystem)
import Effectful.FileSystem.Streaming qualified as EQ
import Effectful.FileSystem.Tagged (doesDirExist, doesFileExist, listDirRecurRel, readFileBinaryLazy)
import Effectful.Log.Extra
import Effectful.Network.Http (Http, RequestBody (..))
import Effectful.Network.S3
import Effectful.Random.Static
import Effectful.Reader.Static
import Effectful.Reader.Static.Lens qualified as EffL
import Effectful.Resource (runResource)
import GHC.Generics
import Lucid
import NeatInterpolation (trimming)
import Network.HTTP.Streaming (toGivesPopperUnlifted)
import Network.Mime
import Network.URI
import Network.URI.Lens (uriPathLens)
import Path.Tagged
import Skylighting.Core
import Skylighting.Format.HTML (formatHtmlBlock, styleToCss)
import System.FilePath.Posix qualified as PosFP
import Text.Blaze.Renderer.Utf8 qualified as BM
import Text.Show.Extra (tshow)

data ReportSeed = ReportSeed
  { repo :: Repo
  , job :: Job
  , pull :: PullRequest
  , commitWorkDir :: PathTo JobCloneDir Abs Dir
  , baseUrl :: Text
  , resources :: Map Text [PathTo ReportResource (RelTo JobCloneDir) File]
  , githubTreeUrl :: Text
  , configFiles :: Map (PathTo ReportResource (RelTo JobCloneDir) File) LBS.ByteString
  , logFiles :: Map (PathTo ReportLogFile (RelTo JobCloneDir) File) LBS.ByteString
  , rawGitOpsConfig :: LBS.ByteString
  , syntaxMap :: SyntaxMap
  }
  deriving (Show, Eq, Ord, Generic)

s3JobReportDir :: (Reader WorkerEnv :> es) => Eff es Text
s3JobReportDir = do
  repoRoot <- s3ReportRepoRootDir
  comm <- EffL.view #commit
  pure $ repoRoot <//> "reports" <//> T.unpack comm.getCommitHash

s3ReportRepoRootDir :: (Reader WorkerEnv :> es) => Eff es Text
s3ReportRepoRootDir = do
  cfg <- EffL.view #repoConfig
  case cfg.report.bucketSubdir of
    Nothing -> pure "/"
    Just subdir -> pure $ T.dropWhileEnd (== '/') subdir <> "/"

infixl 5 <//>

(<//>) :: Text -> String -> Text
(<//>) = fmap T.pack <$> (PosFP.</>) . T.unpack

uploadReport ::
  ( Reader WorkerEnv :> es
  , Concurrent :> es
  , Log :> es
  , Random :> es
  , FileSystem :> es
  , Http :> es
  , IOE :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  Eff es URI
uploadReport = localDomain "upload-report" do
  logInfo_ "Uploading report..."
  cloned <- viewClonedDir
  seed <- generateReportSeed
  cfg <- EffL.view #repoConfig
  basePath <- s3JobReportDir
  runS3 cfg.report.s3 $ do
    logInfo_ "Uploading report index.html..."
    putObject cfg.report.bucket (basePath <//> "index.html") $
      RequestBodyLBS $
        BB.toLazyByteString $
          buildReportHtml seed
    logInfo_ "Report file up"
    logInfo_ "Uploading resources..."
    runResource $ do
      iforM_ seed.resources $ \i xs -> localDomain i $ do
        logInfo_ "Processing resource group..."
        forM_ xs $ \fp -> do
          logInfo_ $ "Uploading: " <> T.pack (fromRelFile fp)
          putObject cfg.report.bucket (basePath <//> fromRelFile fp)
            . RequestBodyStreamChunked
            =<< toGivesPopperUnlifted (EQ.readFile $ cloned </> fp)
        logInfo_ "Resource group uploaded!"
  logInfo_ "All Report uploaded!"
  pure $
    cfg.report.baseUrl
      & uriPathLens
        . TL.packed
        %~ (T.dropWhileEnd (== '/') >>> (<> ("/" <> basePath <> "/index.html")))

updateReportIndex ::
  ( Reader WorkerEnv :> es
  , Concurrent :> es
  , Log :> es
  , Random :> es
  , Http :> es
  , IOE :> es
  ) =>
  Eff es ()
updateReportIndex = localDomain "updateReportIndex" $ do
  cfg <- EffL.view #repoConfig
  repoID <- EffL.view #repo
  (repo, exps) <-
    withGitHubDb $
      Db.notransact $
        do
          repo <-
            fmap fromJust $
              Db.selectFirst $
                Beam.lookup_ gitHubDb.repos $
                  RepoKey repoID
          exps <- Db.selectMany $
            Beam.select $
              orderBy_ (desc_ . Lens.view (_2 . #finishedAt)) $
                do
                  job <-
                    Beam.filter_
                      ( \j ->
                          Beam.isJust_ j.finishedAt
                            &&. (j.status ==. val_ Successed)
                            &&. (j.commit.repo ==. val_ (RepoKey repoID))
                      )
                      $ Beam.all_ gitHubDb.jobs
                  pull <-
                    Beam.related_ gitHubDb.pulls $
                      PullReqKey job.commit.repo job.pullRequest
                  pure (pull, job)
          pure (repo, exps)
  bucketRootPath <- s3ReportRepoRootDir
  logInfo_ "Updating index.html"
  zone <- liftIO getCurrentTimeZone
  runS3 cfg.report.s3 $ do
    putObject cfg.report.bucket (bucketRootPath <//> "index.html")
      . RequestBodyLBS
      . BB.toLazyByteString
      $ toRecentExpsPage zone repo (take 10 exps)
    pure ()
  logInfo_ "Updating all-reports.html"
  runS3 cfg.report.s3 $ do
    putObject cfg.report.bucket (bucketRootPath <//> "all-reports.html")
      . RequestBodyLBS
      . BB.toLazyByteString
      $ allReportsPage zone repo exps
    pure ()
  logInfo_ "Reindexing finished!"

toRecentExpsPage ::
  TimeZone ->
  Repo ->
  [(PullRequest, Job)] ->
  BB.Builder
toRecentExpsPage zone repo jobs =
  let repoName = repo.ownerName <> "/" <> repo.repoName
      title = [trimming|Experiment Reports for ${repoName}|]
   in runIdentity $ execHtmlT $ toStandaloneHtml title $ do
        h1_ $ "Experiment Reports for " <> code_ (toHtml repoName)
        p_ $ toHtml repo.description
        h2_ $ "The Most Recent " <> toHtml (show $ length jobs) <> " Experiment(s)"
        p_ $ "[" <> a_ [href_ "./all-reports.html"] "All Reports" <> "]"
        toReportTable zone jobs

allReportsPage ::
  TimeZone ->
  Repo ->
  [(PullRequest, Job)] ->
  BB.Builder
allReportsPage zone repo jobs =
  let repoName = repo.ownerName <> "/" <> repo.repoName
      title = [trimming|All Experiment Reports for ${repoName}|]
   in runIdentity $ execHtmlT $ toStandaloneHtml title $ do
        h1_ $ "All Experiment Reports for " <> code_ (toHtml repoName)
        p_ $ toHtml repo.description
        h2_ "Experiment Report(s)"
        toReportTable zone jobs

toReportTable :: TimeZone -> [(PullRequest, Job)] -> HtmlT Identity ()
toReportTable zone jobs = table_ $ do
  thead_ $ tr_ $ do
    th_ [scope_ "col"] "Commit"
    th_ [scope_ "col"] "Pull Request"
    th_ [scope_ "col"] "Message"
    th_ [scope_ "col"] "User"
    th_ [scope_ "col"] "JobID"
    th_ [scope_ "col"] "Scheduled"
    th_ [scope_ "col"] "Started"
    th_ [scope_ "col"] "Finished"
  tbody_ $ forM_ jobs $ \(pr, j) -> tr_ $ do
    td_ $ a_ [href_ $ "./reports/" <> j.commit.commit.getCommitHash <> "/index.html"] $ do
      code_ $ toHtml $ T.take 7 j.commit.commit.getCommitHash
    td_ $ do
      "#" <> toHtml (show j.pullRequest) <> ": " <> toHtml pr.title
    td_ $ toHtml j.commitMessage
    td_ $ code_ $ toHtml j.user
    td_ $ code_ $ toHtml j.jobId
    td_ $ toHtml $ fmtTime zone j.scheduledAt
    td_ $ toHtml $ maybe "N/A" (fmtTime zone) j.startedAt
    td_ $ toHtml $ maybe "N/A" (fmtTime zone) j.finishedAt

fmtTime :: TimeZone -> UTCTime -> String
fmtTime zone = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" . utcToZonedTime zone

generateReportSeed ::
  ( Reader WorkerEnv :> es
  , Concurrent :> es
  , Log :> es
  , Random :> es
  , IOE :> es
  , Given (PathTo EtcDir Abs Dir)
  ) =>
  Eff es ReportSeed
generateReportSeed = localDomain "generateSeed" $ do
  logInfo_ "Collecting report data..."
  repoID <- EffL.view #repo
  repoCfg <- EffL.view #repoConfig
  comm <- EffL.view #commit

  msett <-
    withGitHubDb $
      Db.notransact $
        Db.selectFirst $
          selectFullContext repoID comm

  case msett of
    Nothing -> throwM $ NoContextFound repoID comm
    Just FullContext {..} -> do
      let githubTreeUrl =
            "https://github.com/"
              <> repo.ownerName
              <> "/"
              <> repo.repoName
              <> "/tree/"
              <> job.commit.commit.getCommitHash
      commitWorkDir <- viewClonedDir
      syntaxMap <- forM syntaxFiles $ \relfp -> do
        xmlPath <- getAppDataDir <&> (</> syntaxDir </> relfp)
        either throwString pure
          =<< liftIO (parseSyntaxDefinition $ fromAbsFile xmlPath)
      runFileSystem $ do
        resources <- collectResources commitWorkDir
        configFiles <- collectConfigFiles commitWorkDir
        logFiles <- collectLogs commitWorkDir
        rawGitOpsConfig <-
          readFileBinaryLazy $
            commitWorkDir
              </> repoGitOpsConfigYaml
        rootD <- s3ReportRepoRootDir
        let baseUrl =
              T.pack $
                show repoCfg.report.baseUrl
                  PosFP.</> T.unpack rootD
                  PosFP.</> "index.html"
        pure ReportSeed {..}

collectConfigFiles ::
  (Reader WorkerEnv :> es, FileSystem :> es, IOE :> es) =>
  PathTo JobCloneDir Abs Dir ->
  Eff es (Map (PathTo ReportResource (RelTo JobCloneDir) File) LBS.ByteString)
collectConfigFiles cloned = do
  wf <- EffL.view #workflow
  case wf.job of
    Script {} -> pure mempty
    DVCRepro mstages -> getDvcParams cloned mstages

collectResources ::
  (Reader WorkerEnv :> es, FileSystem :> es) =>
  PathTo JobCloneDir Abs Dir ->
  Eff es (Map Text [PathTo ReportResource (RelTo JobCloneDir) File])
collectResources cloned = do
  wf <- EffL.view #workflow
  fmap (Map.unionsWith (<>)) $ forM wf.postDirs $ \dir -> do
    let absDir = cloned </> dir
    there <- doesDirExist absDir
    if there
      then do
        listDirRecurRel absDir <&> \(_, relpaths) ->
          fmap DL.toList
            $ Map.unionsWith (<>)
            $ map
              ( retagPath >>> \relRes ->
                  Map.singleton (parseResourceCategory relRes) $
                    DL.singleton $
                      dir
                        </> relRes
              )
            $ filter
              (not . T.isPrefixOf "." . T.pack . fromRelFile . filename)
              relpaths
      else pure mempty

collectLogs ::
  (Reader WorkerEnv :> es, FileSystem :> es, IOE :> es) =>
  PathTo JobCloneDir Abs Dir ->
  Eff es (Map (PathTo ReportLogFile (RelTo JobCloneDir) File) LBS.ByteString)
collectLogs cloned = do
  repo <- EffL.view #repo
  comm <- EffL.view #commit
  logsD <- getJobLogsDir repo comm
  err <- readIfExists $ logsD </> jobStderrLogFile
  out <- readIfExists $ logsD </> jobStdoutLogFile
  let localLogD = cloned </> [reldir|logs|]
  thereLogs <- doesDirExist localLogD
  logs <-
    if thereLogs
      then map retagPath . snd <$> listDirRecurRel localLogD
      else pure []
  Map.fromList
    . (([relfile|job-stdout.log|], out) :)
    . (([relfile|job-stderr.log|], err) :)
    <$> forM logs \fp ->
      (fp,) <$> readFileBinaryLazy (localLogD </> fp)

readIfExists ::
  (FileSystem :> es) =>
  PathTo e b File ->
  Eff es LBS.ByteString
readIfExists fp = do
  outThere <- doesFileExist fp
  if outThere then readFileBinaryLazy fp else pure "<N/A>"

parseResourceCategory :: PathTo ReportResource (RelTo ReportResourceDir) File -> Text
parseResourceCategory =
  parent >>> dirname >>> fromRelDir >>> T.pack >>> T.dropEnd 1 >>> \case
    "." -> "Other"
    txt ->
      T.unwords $
        filter (not . T.null) $
          map (Lens.ix 0 %~ C.toUpper) $
            T.splitOn "-" txt

toStandaloneHtml :: (Monad m) => Text -> HtmlT m () -> HtmlT m ()
toStandaloneHtml title body = doctypehtml_ $ do
  head_ $ do
    title_ $ toHtml title
    meta_ [charset_ "UTF-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1.0"]
    link_ [rel_ "stylesheet", href_ "https://cdn.simplecss.org/simple.min.css"]
    style_ $ T.pack $ styleToCss pygments
  body_ body

buildReportHtml :: ReportSeed -> BB.Builder
buildReportHtml ReportSeed {..} =
  let theTitle =
        "Result for "
          <> T.take 7 job.commit.commit.getCommitHash
          <> " ("
          <> pull.branch
          <> ")"
          <> " of "
          <> repo.ownerName
          <> "/"
          <> repo.repoName
   in runIdentity $ execHtmlT $ toStandaloneHtml theTitle $ do
        let linkToCommit = a_ [href_ $ repoRoot <> "tree/" <> job.commit.commit.getCommitHash]
            theHeader = do
              "Result: Experiment "
              linkToCommit (code_ (toHtml $ T.take 7 job.commit.commit.getCommitHash))
              " ("
              toHtml job.jobName
              ", "
              toHtml job.jobId
              ")"
            repoRoot =
              "https://github.com/"
                <> repo.ownerName
                <> "/"
                <> repo.repoName
                <> "/"
            pullUrl = repoRoot <> "pull/" <> tshow pull.pullNumber
        h1_ theHeader

        div_ [style_ "font-size: x-large; font-weight: bold;"] $ do
          "[" <> a_ [href_ githubTreeUrl] "View Tree on GitHub" <> "]"
          " "
          "[" <> a_ [href_ baseUrl] "Other Reports" <> "]"

        h2_ "Metadata"
        table_ $ tbody_ $ do
          tr_ $ do
            th_ [scope_ "row"] "Repo"
            td_ $ do
              code_ $ do
                toHtml repo.ownerName
                " / "
                toHtml repo.repoName
              when (repo.description /= "(N/A)") $
                " "
                  <> toHtml repo.description
              " ("
              code_ (toHtml $ show repo.repoID)
              " )"

          tr_ $ do
            th_ [scope_ "row"] "Commit"
            td_ $ linkToCommit $ code_ $ toHtml job.commit.commit.getCommitHash
          tr_ $ do
            th_ [scope_ "row"] "Branch"
            td_ $
              a_ [href_ $ repoRoot <> "tree/" <> pull.branch] $
                code_ $
                  toHtml pull.branch
          tr_ $ do
            th_ [scope_ "row"] "Pull Req"
            td_ $ do
              "#"
              a_ [href_ pullUrl] $
                toHtml (tshow pull.pullNumber)
                  <> " "
                  <> toHtml pull.title
            tr_ $ do
              th_ [scope_ "row"] "Description"
              td_ $ toHtmlRaw $ CMark.commonmarkToHtml [] pull.description

          tr_ $ do
            th_ [scope_ "row"] "Scheduler Job Name and ID"
            td_ $ code_ (toHtml job.jobName) <> " (" <> code_ (toHtml job.jobId) <> ")"
          tr_ $ do
            th_ [scope_ "row"] "Status"
            td_ $ toHtml $ tshow job.status

        iforM_ (Map.delete "Other" resources) $ \cat chs -> do
          h2_ $ toHtml cat
          forM_ chs $ \pth -> do
            let img = T.pack $ fromRelFile pth
            h3_ $ code_ $ toHtml $ fromRelFile $ filename pth
            div_ $ a_ [href_ img] $ case determineFileKind pth of
              (Image, _) -> img_ [src_ img]
              (Video, mtype) ->
                video_ [controls_ ""] $
                  source_ [src_ img, type_ $ T.decodeUtf8 mtype]
              (Unknown, _) -> p_ "Unknown Content"
        forM_ (Map.lookup "Other" resources) $ \chs -> do
          h2_ "Other"
          forM_ chs $ \pth -> do
            let img = T.pack $ fromRelFile pth
            h3_ $ code_ $ toHtml $ fromRelFile $ filename pth
            div_ $ a_ [href_ img] $ img_ [src_ img]
        h2_ "Setting"
        h3_ "Workflow setting"
        tryColorise syntaxMap (Left <$> Map.lookup "YAML" syntaxMap) rawGitOpsConfig
        iforM_ configFiles $ \fp src -> do
          h3_ $ code_ $ toHtml $ fromRelFile fp
          tryColorise syntaxMap (Right <$> fileExtension fp) src
        unless (Map.null logFiles) $ do
          h2_ "Logs"
          iforM_ logFiles $ \pth content -> do
            h3_ $ code_ $ toHtml $ fromRelFile pth
            details_ $ do
              summary_ $ "Show " <> toHtml (fromRelFile pth)
              pre_ $ code_ $ toHtml content

tryColorise :: SyntaxMap -> Maybe (Either Syntax String) -> LBS.ByteString -> Html ()
tryColorise syntaxMap lang rawSrc =
  either
    (const $ pre_ $ code_ $ toHtml rawSrc)
    (toHtmlRaw . BM.renderMarkup . formatHtmlBlock defaultFormatOpts)
    $ do
      syn <-
        maybe (Left "No syntax found") pure $
          either pure (flip lookupSyntax syntaxMap . T.pack)
            =<< lang
      tokenize
        TokenizerConfig {traceOutput = False, syntaxMap}
        syn
        $ LT.toStrict
        $ LT.decodeUtf8 rawSrc

data FileKind = Image | Video | Unknown
  deriving (Show, Eq, Ord, Generic)

determineFileKind :: PathTo b r File -> (FileKind, MimeType)
determineFileKind fp =
  let mtype = defaultMimeLookup $ T.pack $ toFilePath $ filename fp
   in ( if
          | "image/" `BS.isPrefixOf` mtype -> Image
          | "video/" `BS.isPrefixOf` mtype -> Video
          | otherwise -> Unknown
      , mtype
      )