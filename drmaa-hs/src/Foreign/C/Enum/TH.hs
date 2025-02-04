{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Foreign.C.Enum.TH (defineCEnum) where

import Control.DeepSeq
import Control.Monad (zipWithM)
import Data.Maybe
import Foreign (Storable)
import Foreign.C
import Foreign.C.HKD
import GHC.Generics (Generic)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (lift)

defineCEnum :: String -> [String] -> Q [Dec]
defineCEnum tyName enums = do
  newDec <-
    newtypeD
      (pure [])
      (mkName tyName)
      []
      Nothing
      ( normalC
          (mkName tyName)
          [ (Bang NoSourceUnpackedness NoSourceStrictness,)
              <$> [t|CInt|]
          ]
      )
      [ derivClause (Just StockStrategy) [[t|Eq|], [t|Ord|], [t|Generic|]]
      , derivClause
          (Just NewtypeStrategy)
          [[t|Storable|], [t|Num|], [t|Enum|], [t|NFData|]]
      ]
  let theTy = conT (mkName tyName)
      mkPat i c =
        sequenceA
          [ patSynSigD (mkName c) theTy
          , patSynD
              (mkName c)
              (prefixPatSyn [])
              implBidir
              (conP (mkName tyName) [litP $ integerL i])
          ]
      genShowClause i c =
        clause
          [wildP, conP (mkName tyName) [litP $ integerL i]]
          (normalB [|showString $(lift c)|])
          []
  d <- newName "d"
  i <- newName "i"
  let showDec =
        funD
          'showsPrec
          ( zipWith genShowClause [0 ..] enums
              <> [ clause
                    [varP d, conP (mkName tyName) [varP i]]
                    ( normalB
                        [|showParen ($(varE d) > 10) $ showString $(lift tyName) . showChar ' ' . shows $(varE i)|]
                    )
                    []
                 ]
          )
  pats <- concat <$> zipWithM mkPat [0 ..] enums
  packInst <-
    [d|
      instance Packable $theTy where
        type Unpacked_ $theTy = $theTy
        packM c
          | c < 0 = pure Nothing
          | otherwise = pure $ Just c
        unpackM = pure . fromMaybe (-1)
      |]
  comp' <- pragCompleteD [mkName tyName] (Just $ mkName tyName)
  showInst <- instanceD (pure []) [t|Show $theTy|] [showDec]
  comp <- pragCompleteD (map mkName enums) (Just $ mkName tyName)
  pure $ [newDec, showInst, comp, comp'] <> pats <> packInst
