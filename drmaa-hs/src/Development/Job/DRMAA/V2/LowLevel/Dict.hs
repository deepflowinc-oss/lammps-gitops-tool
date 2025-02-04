{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Development.Job.DRMAA.V2.LowLevel.Dict (
  -- ** Lists
  DRMAADict,
  useAsHashMap,
  useAsHashMapMaybe,
  useAsDRMAADict,
  fromDRMAADict,

  -- * More low-level combinators
  createDictWithFinalizer,
  createDict,
  getKeys,
) where

import Control.Exception (bracket, finally)
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as HM
import Data.Maybe (mapMaybe)
import qualified Data.Vector as V
import Development.Job.DRMAA.V2.LowLevel.List (useListAsMaybeVector)
import Development.Job.DRMAA.V2.LowLevel.String
import Development.Job.DRMAA.V2.LowLevel.Types.Basic
import Development.Job.DRMAA.V2.LowLevel.Types.Misc (ReleaseStrategy (..))
import Foreign.C
import Foreign.C.HKD
import Foreign.Ptr (FunPtr, Ptr, nullPtr)
import qualified Language.C.Inline as C hiding (exp, pure)
import qualified Language.C.Inline.Interruptible as C

C.context (C.baseCtx <> C.funCtx <> C.bsCtx <> C.fptrCtx <> drmaa2BasicTypeCtx)
C.include "<drmaa2.h>"

createDictWithFinalizer ::
  FunPtr (Ptr CString -> Ptr CString -> IO ()) ->
  IO DRMAADict
createDictWithFinalizer freer =
  [C.exp|drmaa2_dict {drmaa2_dict_create($(void (*freer)(char **, char**)))}|]

createDict :: IO DRMAADict
createDict =
  createDictWithFinalizer
    [C.funPtr|void freePair(char **key, char **entry){
      if (key != NULL) {
        // drmaa2_string_free(key); 
      }
      if (entry != NULL) {
        // drmaa2_string_free(entry);
      }
    }|]

useAsDRMAADict ::
  ReleaseStrategy ->
  HM.HashMap BS.ByteString BS.ByteString ->
  (DRMAADict -> IO a) ->
  IO a
useAsDRMAADict strat hm = bracket alloc release
  where
    alloc = do
      d <- createDictWithFinalizer [C.funPtr|void freeNothing(char **k, char **v){}|]
      mapM_ (uncurry $ setItem d) (HM.toList hm)
      pure d
    release dict =
      case strat of
        Free -> [C.exp|void { drmaa2_dict_free(&$(drmaa2_dict dict)) }|]
        NoFree -> pure ()

setItem :: DRMAADict -> BS.ByteString -> BS.ByteString -> IO ()
setItem dict k v =
  [C.exp|void { drmaa2_dict_set($(const drmaa2_dict dict), $bs-cstr:k, $bs-cstr:v) }|]

fromDRMAADict :: ReleaseStrategy -> DRMAADict -> IO (Maybe (HM.HashMap BS.ByteString BS.ByteString))
fromDRMAADict strat dict = useAsHashMapMaybe strat dict pure

useAsHashMap :: ReleaseStrategy -> DRMAADict -> (HM.HashMap BS.ByteString BS.ByteString -> IO a) -> IO a
useAsHashMap strat dict@(DRMAADict ptr) k
  | ptr == nullPtr = k mempty
  | otherwise = flip finally release $ do
      keys <- getKeys dict
      useListAsMaybeVector Free keys $ \case
        Nothing -> k mempty
        Just ks -> do
          ents <- V.forM ks \l ->
            (l,) <$> getItem dict l
          k $ HM.fromList $ mapMaybe sequenceA $ V.toList ents
  where
    release = case strat of
      Free -> [C.exp|void { drmaa2_dict_free(&$(drmaa2_dict dict)) }|]
      NoFree -> pure ()

useAsHashMapMaybe ::
  ReleaseStrategy ->
  DRMAADict ->
  ( Maybe (HM.HashMap BS.ByteString BS.ByteString) ->
    IO a
  ) ->
  IO a
useAsHashMapMaybe strat dict@(DRMAADict ptr) k
  | ptr == nullPtr = k Nothing
  | otherwise = flip finally release $ do
      keys <- getKeys dict
      useListAsMaybeVector Free keys $ \case
        Nothing -> k mempty
        Just ks -> do
          ents <- V.forM ks \l ->
            (l,) <$> getItem dict l
          k $ Just $ HM.fromList $ mapMaybe sequenceA $ V.toList ents
  where
    release = case strat of
      Free -> [C.exp|void { drmaa2_dict_free(&$(drmaa2_dict dict)) }|]
      NoFree -> pure ()

getItem :: DRMAADict -> BS.ByteString -> IO (Maybe BS.ByteString)
getItem dict k =
  [C.exp|const char * {drmaa2_dict_get($(drmaa2_dict dict), $bs-cstr:k)}|] >>= \ptr ->
    if ptr == nullPtr
      then pure Nothing
      else Just <$> BS.packCString ptr

getKeys :: DRMAADict -> IO (DRMAAList DRMAAString)
getKeys dict =
  [C.exp|drmaa2_string_list { 
    drmaa2_dict_list($(drmaa2_dict dict))
  }|]

instance Packable (HM.HashMap BS.ByteString BS.ByteString) where
  type Unpacked_ (HM.HashMap BS.ByteString BS.ByteString) = DRMAADict
  packM = fromDRMAADict NoFree
  unpackM Nothing = pure $ DRMAADict nullPtr
  unpackM (Just hm) = useAsDRMAADict NoFree hm pure
