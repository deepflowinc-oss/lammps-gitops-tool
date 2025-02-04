{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}

module Development.LAMMPS.GitOps.API.Client (
  restClient,
  deployBinaries,
) where

import Data.Functor (void)
import Data.Generics.Labels ()
import Data.Proxy (Proxy (..))
import Development.LAMMPS.GitOps.API.Types
import Servant.API
import Servant.Auth.Client
import Servant.Client
import Streaming.ByteString qualified as Q

restClient :: RestApi (AsClientT ClientM)
restClient = client (Proxy @(NamedRoutes RestApi))

deployBinaries :: Token -> Q.ByteStream IO () -> ClientM ()
deployBinaries = fmap void . deploy
  where
    RestApi {..} = restClient
    DeployApi {..} = deployApi
