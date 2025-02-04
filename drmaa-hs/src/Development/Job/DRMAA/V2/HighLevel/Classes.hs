{-# LANGUAGE FlexibleInstances #-}

module Development.Job.DRMAA.V2.HighLevel.Classes (
  Nullable (..),
  fromNullable,
  toNullable,
) where

import Data.Maybe (fromMaybe)
import qualified Development.Job.DRMAA.V2.LowLevel.Types as Raw
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Enums
import Foreign
import Foreign.C

class (Eq a) => Nullable a where
  unsetValue :: a

instance Nullable CBool where
  unsetValue = 0

instance Nullable DRMAAString where
  unsetValue = DRMAAString nullPtr

instance Nullable (DRMAAList a) where
  unsetValue = DRMAAList nullPtr

instance Nullable DRMAADict where
  unsetValue = DRMAADict nullPtr

instance Nullable CInt where
  unsetValue = -1

instance Nullable CLong where
  unsetValue = -1

instance Nullable CShort where
  unsetValue = -1

instance Nullable Error where
  unsetValue = -1

instance Nullable JobState where
  unsetValue = -1

instance Nullable OS where
  unsetValue = OS (-1)

instance Nullable CPU where
  unsetValue = CPU (-1)

instance Nullable DRMAAListType where
  unsetValue = -1

instance Nullable (FunPtr a) where
  unsetValue = nullFunPtr

instance Nullable Raw.JobInfo where
  unsetValue = Raw.MkJobInfo nullPtr

instance Nullable CTime where
  unsetValue = -1

fromNullable :: (Nullable a) => a -> Maybe a
fromNullable a
  | a == unsetValue = Nothing
  | otherwise = Just a

toNullable :: (Nullable a) => Maybe a -> a
toNullable = fromMaybe unsetValue
