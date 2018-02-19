{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.IO.Huck.Parser where

import           Control.Lens
import           Control.Monad.Trans.Resource (runResourceT)

import qualified Data.Text.IO as T

import           Hedgehog
import           Huck.Prelude
import           Huck.Lenses

import           System.IO (IO)

import           Test.Huck.Util

prop_parse_examples =
  let file = "test/data/supported.toml" in
  withTests 1 . property . test . runResourceT $ do
    code <- liftIO $ T.readFile file
    tokens <- evalEither . lex $ code
    void . evalEither . parse $ tokens

prop_lenses =
  let file = "test/data/supported.toml" in
  withTests 1 . property .test . runResourceT $ do
    str <- liftIO $ T.readFile file
    r <-  evalEither $ parseText str

    (r ^? key "owner" . _TTable . ix "name" . _TString,
     r ^? key "servers.alpha" . _TTable . ix "ip" . _TString,
     r ^? key "servers.beta" . _TTable . ix "ip" . _TString) ===
      (Just "\"Lance Uppercut\"", Just "\"10.0.0.1\"", Just "\"10.0.0.2\"")

-- TODO This should work after naming of nested tables is fixed
--  r ^? key "servers" . _TTable . ix "alpha" . _TTable . ix "ip" . _TString === Just "10.0.0.1"

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
