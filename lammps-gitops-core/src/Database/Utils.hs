{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Database.Utils (
  Shown (..),
  Reading (..),
  parseSqliteTextFieldWith,
) where

import Control.Monad ((<=<))
import qualified Data.Text as T
import Database.Beam (FromBackendRow)
import qualified Database.Beam.Sqlite as Beam
import qualified Database.SQLite.Simple.FromField as Sqlite
import qualified Database.SQLite.Simple.ToField as Sqlite
import GHC.Generics (Generic)
import Text.Read (readEither)
import Type.Reflection (Typeable)

newtype Shown a = Shown {runShownField :: a}
  deriving (Show, Eq, Ord, Generic)

instance (Show a) => Sqlite.ToField (Shown a) where
  toField = Sqlite.toField . show . runShownField
  {-# INLINE toField #-}

newtype Reading a = Reading a
  deriving (Eq, Ord, Generic)
  deriving newtype (Read)

instance (Read a) => Sqlite.FromField (Reading a) where
  fromField = parseSqliteTextFieldWith $ fmap Reading . readEither . T.unpack
  {-# INLINE fromField #-}

deriving anyclass instance (Read a, Typeable a) => FromBackendRow Beam.Sqlite (Reading a)

parseSqliteTextFieldWith :: (T.Text -> Either String a) -> Sqlite.FieldParser a
parseSqliteTextFieldWith p = either fail pure . p <=< Sqlite.fromField
