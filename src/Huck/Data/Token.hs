{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
module Huck.Data.Token (
    Tokens (..)
  , Token (..)
  , renderToken
  , STRING (..)
  , renderString
  ) where

import qualified Data.List.NonEmpty as NonEmpty
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Builder as Lazy
import qualified Data.Text.Lazy.Builder.Int as Lazy
import qualified Data.Text.Lazy.Builder.RealFloat as Lazy
import           Data.Time (TimeZone, UTCTime (..), utcToZonedTime)
import qualified Data.Time.RFC3339 as RFC3339

import           Huck.Prelude

import           Text.Megaparsec (ShowToken (..))

newtype Tokens = Tokens { unTokens :: [Token] }
  deriving (Show, Eq)

data Token
  = BOOL Bool
  | INTEGER Int64
  | FLOAT Double
  | STRING STRING
  | DATE (UTCTime, TimeZone)
  | COMMENT Text
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | EQUAL
  | EOF
  | COMMA
  | DOT
  deriving (Show, Eq, Ord)

data STRING
  = RAW Text
  | BASIC Text
  | BASIC_MULTI Text
  | LITERAL Text
  | LITERAL_MULTI Text
  deriving (Show, Eq, Ord)

renderToken :: Token -> Text
renderToken = \case
  BOOL True -> "true"
  BOOL False -> "false"
  INTEGER i -> renderInt i
  FLOAT d -> renderFloat d
  STRING r -> renderString r
  DATE (u, tz) -> renderDate u tz
  LBRACK -> "["
  RBRACK -> "]"
  LBRACE -> "{"
  RBRACE -> "}"
  EQUAL -> "="
  EOF -> ""
  COMMA -> ","
  COMMENT s -> "#" <> s <> "\n"
  DOT -> "."

renderDate :: UTCTime -> TimeZone -> Text
renderDate u tz = RFC3339.formatTimeRFC3339 $ utcToZonedTime tz u

renderInt :: Int64 -> Text
renderInt = fromBuilder . Lazy.decimal

renderFloat :: Double -> Text
renderFloat = fromBuilder . Lazy.realFloat

renderString :: STRING -> Text
renderString = \case
  RAW s -> s
  BASIC s -> "\"" <> s <> "\""
  BASIC_MULTI ss -> "\"\"\"" <> ss <> "\"\"\""
  LITERAL ss -> "'" <> ss <> "'"
  LITERAL_MULTI ss -> "'''" <> ss  <> "'''"

fromBuilder :: Lazy.Builder -> Text
fromBuilder =
  Lazy.toStrict . Lazy.toLazyText

instance ShowToken Token where
  showTokens =
    T.unpack . T.unwords . fmap renderToken . NonEmpty.toList
