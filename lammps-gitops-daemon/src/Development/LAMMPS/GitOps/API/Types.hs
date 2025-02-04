{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoFieldSelectors #-}

module Development.LAMMPS.GitOps.API.Types (
  RestApi (..),
  restAPI,
  RepoApi (..),
  CommitApi (..),
  PullApi (..),
  DeployApi (..),
  AuthUser (..),
  signAuthUser,
  JobDescriptor (..),
) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import Data.Proxy
import Data.Text qualified as T
import Development.LAMMPS.GitOps.App.Daemon.Model
import Development.LAMMPS.GitOps.Types
import GHC.Generics (Generic)
import Servant.API
import Servant.Auth.Server
import Streaming.ByteString qualified as Q
import Streaming.Servant.Orphans ()

restAPI :: Proxy (ToServantApi RestApi)
restAPI = genericApi $ Proxy @RestApi

data JobDescriptor = JobDescriptor {pull :: PullRequest, job :: Job}
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON, ToJSON, NFData)

data RestApi mode = RestApi
  { deployApi :: mode :- "api" :> "deploy" :> NamedRoutes DeployApi
  , repoApi ::
      mode :- "api" :> "repos" :> Capture "ownerName" T.Text :> Capture "repoName" T.Text :> NamedRoutes RepoApi
  }
  deriving (Generic)

newtype DeployApi mode = DeployApi
  { deploy ::
      mode
        :- Auth '[JWT] AuthUser
        :> StreamBody NoFraming OctetStream (Q.ByteStream IO ())
        :> Put '[JSON] NoContent
  }
  deriving (Generic)

data RepoApi mode = RepoApi
  { listAllJobs :: mode :- Get '[JSON] [JobDescriptor]
  , commitApi :: mode :- "commits" :> Capture "commit" CommitHash :> NamedRoutes CommitApi
  , pullApi :: mode :- "pulls" :> Capture "pullID" IssueNumber :> NamedRoutes PullApi
  }
  deriving (Generic)

data CommitApi mode = CommitApi
  { getCommitJob :: mode :- Get '[JSON] JobDescriptor
  , scheduleCommitJob :: mode :- ReqBody '[JSON] IssueNumber :> Post '[JSON] JobDescriptor
  , cancelCommitJob :: mode :- Delete '[JSON] JobDescriptor
  }
  deriving (Generic)

data PullApi mode = PullApi
  { listPullJobs :: mode :- Get '[JSON] [JobDescriptor]
  , schedulePullNewestCommitJob :: mode :- Post '[JSON] JobDescriptor
  , cancelAllPullJobs :: mode :- Delete '[JSON] [JobDescriptor]
  , cancelMostRecentPullJob :: mode :- "latest" :> Delete '[JSON] JobDescriptor
  }
  deriving (Generic)
