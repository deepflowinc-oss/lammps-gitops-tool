{-# LANGUAGE TemplateHaskell #-}

module Development.Job.DRMAA.V2.LowLevel.ImplementationSpecific.TH (defineImplSpecFunc) where

import qualified Data.ByteString as BS
import Data.IORef
import qualified Data.Vector as V
import qualified Language.C.Inline.Interruptible as C
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import System.IO.Unsafe

-- NOTE: It seems athat we CANNOT call @drmaa2_*_impl_spec@ multiple times
-- as it results in SEGV. It seems that drmaa2-python also caches result.
defineImplSpecFunc :: String -> String -> DecsQ
defineImplSpecFunc name actQ = do
  rawFunName <- newName $ name <> "_raw"
  rawDs <-
    sequenceA
      [ sigD rawFunName [t|IO (Maybe (V.Vector BS.ByteString))|]
      , funD
          rawFunName
          [ clause [] (normalB [|fromDRMAAListMaybe NoFree $(quoteExp C.pure actQ)|]) []
          ]
      ]
  refName <- newName $ name <> "_ref"
  refDs <-
    sequenceA
      [ sigD refName [t|IORef (Maybe (Maybe (V.Vector BS.ByteString)))|]
      , pragInlD refName NoInline FunLike AllPhases
      , funD refName [clause [] (normalB [|unsafePerformIO $ newIORef Nothing|]) []]
      ]
  let bodyName = mkName name
  bodyDs <-
    sequenceA
      [ sigD bodyName [t|Maybe (V.Vector BS.ByteString)|]
      , pragInlD bodyName NoInline FunLike AllPhases
      , funD
          bodyName
          [ clause
              []
              ( normalB
                  [|
                    unsafeDupablePerformIO
                      $ atomicModifyIORef' $(varE refName) (maybe (join ((,) . Just) $! unsafePerformIO $(varE rawFunName)) (join ((,) . Just)))
                    |]
              )
              []
          ]
      ]
  pure $ rawDs <> refDs ++ bodyDs
