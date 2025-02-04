{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..)) where

import Control.DeepSeq (NFData)
import Data.Hashable (Hashable)
import GHC.Generics (Generic)

data ReleaseStrategy = Free | NoFree
  deriving (Show, Eq, Ord, Generic)
  deriving anyclass (NFData, Hashable)
