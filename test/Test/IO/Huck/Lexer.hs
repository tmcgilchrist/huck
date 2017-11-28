{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Huck.Lexer where

import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Text.IO as T

import           Hedgehog
import           Huck.Prelude

import           System.IO (IO)

import           Test.Huck.Util

prop_lexer_examples =
  let file = "test/data/supported.toml" in
  property . test . runResourceT $ do
    code <- liftIO $ T.readFile file
    void . evalEither . lex $ code

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
