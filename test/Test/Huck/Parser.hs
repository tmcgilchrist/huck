{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Parser where

import           Hedgehog

import           Huck.Prelude

import           System.IO (IO)
import           Test.Huck.Gen
import           Test.Huck.Util

prop_parser_roundtrips = property $ do
  code <- forAll genParsableText
  tokens <- evalEither . lex $ code
  void . evalEither . parse $ tokens

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
