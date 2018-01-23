{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Huck.Parser where

import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Text.IO as T

import           Hedgehog
import           Huck.Prelude

import           System.IO (IO)

import           Test.Huck.Util

prop_parse_examples =
  let file = "test/data/supported.toml" in
  property . test . runResourceT $ do
    code <- liftIO $ T.readFile file
    tokens <- evalEither . lex $ code
    void . evalEither . parse $ tokens

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
