{-# LANGUAGE NoImplicitPrelude #-}
module Huck.Pretty (
    ppTomlDoc
  ) where

import           Data.Text

import           Huck.Data
import           Huck.Position
import           Huck.Prelude

import qualified Text.PrettyPrint.Annotated.WL as PP

ppTomlDoc :: TomlDocument Position -> Text
ppTomlDoc doc = undefined
