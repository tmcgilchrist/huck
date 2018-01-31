{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Lexer where

import           Data.Text (Text)
import qualified Data.Text as T

import           Hedgehog

import           Huck.Data
import           Huck.Data.Token
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
  tripping tokens printTokens (\i -> (fmap . fmap) (\((:@) j _) -> j) $ lex i)

  where
    printTokens :: [Token] -> Text
    printTokens = T.intercalate " " . fmap renderToken

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
