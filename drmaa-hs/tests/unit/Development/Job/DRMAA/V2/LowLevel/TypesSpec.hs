{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}

module Development.Job.DRMAA.V2.LowLevel.TypesSpec (test_JobTemplate) where

import Barbies
import Control.Monad (filterM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Function (on)
import Data.Functor.Const (Const (..))
import Data.Maybe
import Data.Monoid (All (..))
import qualified Data.Vector as V
import Development.Job.DRMAA.V2.HighLevel.Job
import Development.Job.DRMAA.V2.LowLevel (createJobTemplate)
import Development.Job.DRMAA.V2.LowLevel.ImplementationSpecific
import qualified Development.Job.DRMAA.V2.LowLevel.Types as T
import Development.Job.DRMAA.V2.Managed (Managed (..))
import Foreign
import GHC.Generics (Generic)
import GHC.Records (HasField (..))
import Test.Tasty
import Test.Tasty.QuickCheck

newtype AsciiStr = AsciiStr {getAsciiStr :: BS.ByteString}
  deriving (Show, Eq, Ord, Generic)

instance Arbitrary AsciiStr where
  arbitrary = do
    name <- listOf1 $ elements $ ['a' .. 'z'] ++ ['A' .. 'Z']
    pure $ AsciiStr $ BS8.pack name
  shrink (AsciiStr t)
    | BS.length t <= 1 = []
    | otherwise =
        map (AsciiStr . BS8.pack) $ filter (not . null) $ filterM (const [True, False]) $ BS8.unpack t

class EqModNull a where
  infix 4 ~~
  (~~) :: Maybe a -> Maybe a -> Bool

instance {-# OVERLAPPABLE #-} (Eq a) => EqModNull a where
  (~~) = (==)

instance {-# OVERLAPPING #-} EqModNull Bool where
  (~~) = (==) `on` fromMaybe False

infix 4 ~~~

(~~~) ::
  ( AllB EqModNull f
  , ConstraintsB f
  , ApplicativeB f
  , TraversableB f
  , Show (f Maybe)
  ) =>
  f Maybe ->
  f Maybe ->
  Property
l ~~~ r =
  counterexample
    (show l <> "/~" <> show r)
    $ getAll
    $ bfoldMap (All . getConst)
    $ bzipWithC @EqModNull (fmap Const . (~~)) l r

test_JobTemplate :: TestTree
test_JobTemplate =
  testGroup
    "JobTemplate"
    [ testGroup
        "withUnpackedMoved . withUnpackedMoved ≈ id (mod unset value)"
        [ testProperty "Only jobName and remoteCommand are specified" $
            \(AsciiStr jname) (AsciiStr cmd) ->
              let tmplt = (mempty @JobTemplate) {payload = mempty {jobName = Just jname, remoteCommand = Just cmd}}
               in checkPackUnpack tmplt
        , testProperty "jobName, remoteCommand, and args specified" $
            \(AsciiStr jname) (AsciiStr cmd) args ->
              let tmplt =
                    (mempty @JobTemplate)
                      { payload =
                          mempty
                            { jobName = Just jname
                            , remoteCommand = Just cmd
                            , args = Just $ V.fromList $ map getAsciiStr args
                            }
                      }
               in checkPackUnpack tmplt
        ]
    ]

checkPackUnpack :: JobTemplate -> Property
checkPackUnpack tmplt = ioProperty
  $ withUnpackedWithImplSpecMoved @JobTemplateF
    (getField @"getJobTemplate" <$> createJobTemplate)
    tmplt
  $ \(Managed unpacked) -> withForeignPtr unpacked $ \ptr -> do
    repacked <- bpackStructWithImplSpec ptr
    pure $ repacked ~~~ tmplt
