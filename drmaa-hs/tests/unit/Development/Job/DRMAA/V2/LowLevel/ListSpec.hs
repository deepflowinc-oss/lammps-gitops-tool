module Development.Job.DRMAA.V2.LowLevel.ListSpec (
  test_toDRMAAList,
  test_from_to_List,
) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V
import Development.Job.DRMAA.V2.LowLevel.List
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import Foreign
import Test.Tasty
import Test.Tasty.QuickCheck

test_toDRMAAList :: TestTree
test_toDRMAAList =
  testGroup
    "toDRMAAList"
    [ testProperty "never generate NULL" $ \strs ->
        let bss = V.fromList $ map (TE.encodeUtf8 . T.pack . getPrintableString) strs
         in tabulate "length" [show $ V.length bss] $ ioProperty $ do
              DRMAAList ptr <- toDRMAAList bss
              pure $ ptr =/= nullPtr
    , testProperty "getListSize <=< toDRMAAList ≡ V.length" $ \strs ->
        let bss = V.fromList $ map (TE.encodeUtf8 . T.pack . getPrintableString) strs
         in tabulate "length" [show $ V.length bss] $ ioProperty $ do
              len <- getListSize =<< toDRMAAList bss
              pure $ fromIntegral len === V.length bss
    ]

test_from_to_List :: TestTree
test_from_to_List =
  testGroup
    "fromDRMAAList . toDRMAAList ≡ id"
    [ testProperty "Free policy" $ checkFromTo Free
    , testProperty "NoFree policy" $ checkFromTo NoFree
    ]

checkFromTo :: ReleaseStrategy -> [PrintableString] -> Property
checkFromTo strat strs =
  let bss = V.fromList $ TE.encodeUtf8 . T.pack . getPrintableString <$> strs
   in tabulate "length" [show $ V.length bss `div` 10 * 10] $ ioProperty $ do
        fromTo <- fromDRMAAList strat =<< toDRMAAList bss
        pure $ fromTo === bss
