{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Huck.Pretty (
    ppTomlDoc
  , ppToml
  , ppDateTime
  , ppTable
  , ppArray
  , ppBoolean
  , ppFloat
  , ppString
  , ppInteger64
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime, TimeZone, utcToZonedTime)
import qualified Data.Time.RFC3339 as RFC3339
import           Data.Vector (Vector)

import           Huck.Data
import           Huck.Prelude

import           Text.PrettyPrint.Annotated.WL (Doc, (<+>))
import qualified Text.PrettyPrint.Annotated.WL as PP

ppTomlDoc :: TomlDocument a -> Doc a
ppTomlDoc doc = ppTable (tomlDocument doc)

ppToml :: Toml a -> Doc a
ppToml toml =
  case toml of
    TString _ t -> ppString t
    TInteger _ i -> ppInteger64 i
    TFloat _ i -> ppFloat i
    TBoolean _ b -> ppBoolean b
    TArray _ a -> ppArray a
    TTable _ t -> ppTable t
    TDatetime _ utc tz -> ppDateTime tz utc
    TComment _ s -> PP.text $ "#" <> T.unpack s

ppDateTime :: TimeZone -> UTCTime -> Doc a
ppDateTime tz =
  PP.text . RFC3339.formatTimeRFC3339 . utcToZonedTime tz

ppTable :: HashMap Text (Toml a) -> Doc a
ppTable = HM.foldrWithKey (\key toml doc -> ppString key <+> PP.equals <+> ppToml toml <+> PP.line <+> doc) ""

ppArray :: Vector (Toml a) -> Doc a
ppArray a =
  PP.encloseSep PP.lbracket PP.rbracket PP.comma (fmap ppToml a)

ppBoolean :: Bool -> Doc a
ppBoolean = bool (PP.text "false") (PP.text "true")

ppFloat :: Double -> Doc a
ppFloat = PP.pretty

ppString :: Text -> Doc a
ppString = PP.text . T.unpack

ppInteger64 :: Int64 -> Doc a
ppInteger64 = PP.pretty

-- TODO
-- 1. What to do with the position information? do we use it to guide the output formatting?
-- look at github.com/qfpl/sv for example of a textual parser that preserves spaces/formatting
