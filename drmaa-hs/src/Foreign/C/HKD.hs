{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}

module Foreign.C.HKD (
  Packable (..),
  Unpacked (..),
  Duration (..),
  bpackStruct,
  withUnpackedPtr,
) where

import Barbies
import Barbies.Constraints ()
import Control.DeepSeq (NFData, force)
import Control.Exception (evaluate)
import Control.Monad ((<=<))
import Data.Hashable (Hashable)
import Data.Int (Int64)
import Data.Kind (Type)
import Data.Maybe (fromMaybe)
import Data.Time (NominalDiffTime, UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Foreign (with)
import Foreign.C
import Foreign.Ptr (Ptr)
import Foreign.Storable
import GHC.Generics (Generic)

newtype Unpacked a = Unpacked {getUnpacked :: Unpacked_ a}
  deriving (Generic)

deriving newtype instance (Show (Unpacked_ a)) => Show (Unpacked a)

deriving newtype instance (Eq (Unpacked_ a)) => Eq (Unpacked a)

deriving newtype instance (Ord (Unpacked_ a)) => Ord (Unpacked a)

deriving newtype instance (Hashable (Unpacked_ a)) => Hashable (Unpacked a)

deriving newtype instance (Storable (Unpacked_ a)) => Storable (Unpacked a)

class (NFData a, Storable (Unpacked_ a)) => Packable a where
  type Unpacked_ a = (r :: Type) | r -> a
  packM :: Unpacked_ a -> IO (Maybe a)
  unpackM :: Maybe a -> IO (Unpacked_ a)

instance Packable Bool where
  type Unpacked_ Bool = CBool
  packM 0 = pure $ Just False
  packM 1 = pure $ Just True
  packM _ = pure Nothing
  unpackM p = pure $ if fromMaybe False p then 1 else 0

instance Packable Int where
  type Unpacked_ Int = CInt
  packM = pure . \i -> if i < 0 then Nothing else Just $ fromIntegral i
  unpackM = pure . maybe (-1) fromIntegral

instance Packable UTCTime where
  type Unpacked_ UTCTime = CTime
  packM =
    pure . \i ->
      if i == -3
        then Nothing
        else Just $ posixSecondsToUTCTime $ realToFrac i
  unpackM = pure . maybe (-3) (fromIntegral @Int . floor . utcTimeToPOSIXSeconds)

instance Packable Int64 where
  type Unpacked_ Int64 = CLong
  packM =
    pure . \i ->
      if i == -1
        then Nothing
        else Just $ fromIntegral i
  unpackM = pure . maybe (-1) fromIntegral

newtype Duration = Duration {getDuration :: CTime}
  deriving (Show, Eq, Ord, Generic)
  deriving newtype (Num, Storable, Real)

instance Packable NominalDiffTime where
  type Unpacked_ NominalDiffTime = Duration
  packM = pure . \i -> if i == -3 then Nothing else Just $ realToFrac i
  unpackM = pure . maybe (-3) (fromIntegral @Int . floor)

bpackStruct ::
  ( TraversableB struct
  , ConstraintsB struct
  , Storable (struct Unpacked)
  , AllB Packable struct
  , NFData (struct Maybe)
  ) =>
  Ptr (struct Unpacked) ->
  IO (struct Maybe)
bpackStruct ptr =
  evaluate . force
    =<< btraverseC @Packable (evaluate . force <=< packM . getUnpacked)
    =<< peek ptr

withUnpackedPtr ::
  ( TraversableB struct
  , ConstraintsB struct
  , Storable (struct Unpacked)
  , AllB Packable struct
  ) =>
  struct Maybe ->
  (Ptr (struct Unpacked) -> IO a) ->
  IO a
withUnpackedPtr struct k = do
  unpacked <- btraverseC @Packable (fmap Unpacked . unpackM) struct
  with unpacked k
