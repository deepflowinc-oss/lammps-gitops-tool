{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Network.URI.Orphans () where

import Data.Aeson
import Data.Hashable (Hashable)
import qualified Data.Text as T
import Network.URI

instance FromJSON URI where
  parseJSON = withText "valid uri" $ maybe (fail "Invalid URI") pure . parseURI . T.unpack

instance ToJSON URI where
  toJSON = toJSON . show

deriving instance Hashable URI

deriving instance Hashable URIAuth
