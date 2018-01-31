{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Parser where

import qualified Data.Text as T

import           Hedgehog

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
  str <- forAll genToml
  tripping str -- Generated TomlDocument as Text
           (\f -> T.pack . display . renderPrettyDefault . ppTomlDoc $ f)
           (parseText)

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
