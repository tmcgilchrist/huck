{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Parser where

import qualified Data.Text as T

import           Hedgehog

import           Huck.Data
import           Huck.Parser
import           Huck.Prelude
import           Huck.Pretty

import           System.IO (IO)

import           Test.Huck.Gen
import           Test.Huck.Util
import           Text.PrettyPrint.Annotated.WL

prop_parser_roundtrips = property $ do
  code <- forAll genParsableText
  tokens <- evalEither . lex $ code
  void . evalEither . parse $ tokens

prop_parser_tripping = property $ do
  str <- forAll genTomlDoc
  tripping (stripPositions str)
           (T.pack . display . renderPrettyDefault . ppTomlDoc)
           (second stripPositions . parseText)

prop_date_tripping = property $ do
  date <- forAll genDateToml
  tripping date
           (T.pack . display . renderPrettyDefault . ppToml)
           (second stripPosition . parseSnippet pLitDate)

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
