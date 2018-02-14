{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Lexer where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Huck.Prelude
import           Huck.Position

import           System.IO (IO)

import           Test.Huck.Gen
import           Test.Huck.Util

prop_lexer_roundtrip_samples = property $ do
  code <- forAll genLexableTexts
  void . evalEither . lex $ code

prop_lexer_roundtrip_tokens = property $ do
  tokens <- forAll genTokens
  tripping tokens printTokens ((fmap . fmap) (\((:@) j _) -> j) . lex )

prop_lexer_date_roundtrip = property $ do
  date <- forAll (Gen.list (Range.linear 0 10) genDateToken)
  tripping date printTokens ((fmap . fmap) (\((:@) j _) -> j) . lex)

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
