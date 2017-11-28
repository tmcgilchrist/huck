{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Lexer where

import           Hedgehog

import           Huck.Prelude

import           System.IO (IO)
import           Test.Huck.Gen
import           Test.Huck.Util

prop_lexer_roundtrip = property $ do
  code <- forAll genLexableText
  void . evalEither . lex $ code

prop_lexer_roundtrips = property $ do
  code <- forAll genLexableTexts
  void . evalEither . lex $ code

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
