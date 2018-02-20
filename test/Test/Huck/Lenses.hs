{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Lenses where

import           Huck.Prelude

import           Hedgehog

-- TODO Using hedgehog-checkers assert the behaviour of lenses

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
