module Development.Job.DRMAA.V2.LowLevel.StringSpec (
  test_from_to_String,
  test_fromDRMAAStringMaybe,
  test_toDRMAAString,
) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Development.Job.DRMAA.V2.LowLevel.String
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import Foreign (nullPtr)
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

test_from_to_String :: TestTree
test_from_to_String =
  testGroup
    "fromDRMAAString . toDRMAAString ≡ id"
    [ testProperty "Free policy" $ checkFromTo Free
    , testProperty "NoFree policy" $ checkFromTo NoFree
    ]

test_toDRMAAString :: TestTree
test_toDRMAAString =
  testGroup
    "toDRMAAString"
    [ testProperty "toDRMAAString ≢ NULL" $
        \(PrintableString str) -> ioProperty $ do
          let bs = TE.encodeUtf8 $ T.pack str
          DRMAAString ptr <- toDRMAAString bs
          pure $ ptr =/= nullPtr
    ]

test_fromDRMAAStringMaybe :: TestTree
test_fromDRMAAStringMaybe =
  testGroup
    "test_fromDRMAAStringMaybe"
    [ testGroup
        "fromDRMAAStringMaybe . toDRMAAString ≡ Just"
        [ testProperty "Free policy" $ checkFromMaybeTo Free
        , testProperty "NoFree policy" $ checkFromMaybeTo NoFree
        ]
    , testCaseSteps "fromDRMAAStringMaybe NULL ≡ Nothing" $ \step -> do
        step "NoFree"
        nofree <- fromDRMAAStringMaybe NoFree $ DRMAAString nullPtr
        nofree @?= Nothing
        step "Free"
        free <- fromDRMAAStringMaybe Free $ DRMAAString nullPtr
        free @?= Nothing
    ]

checkFromTo :: ReleaseStrategy -> PrintableString -> Property
checkFromTo strat (PrintableString str) =
  let bs = TE.encodeUtf8 $ T.pack str
   in tabulate "length" [show $ BS8.length bs `div` 10 * 10] $ ioProperty $ do
        fromTo <- fromDRMAAString strat =<< toDRMAAString bs
        pure $ fromTo === bs

checkFromMaybeTo :: ReleaseStrategy -> PrintableString -> Property
checkFromMaybeTo strat (PrintableString str) =
  let bs = TE.encodeUtf8 $ T.pack str
   in tabulate "length" [show $ BS8.length bs `div` 10 * 10] $ ioProperty $ do
        fromTo <- fromDRMAAStringMaybe strat =<< toDRMAAString bs
        pure $ fromTo === Just bs
