{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}
module Huck.Parser.Data (
    Parser
  , ParserError (..)
  , renderParserError
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Huck.Data
import           Huck.Position
import           Huck.Prelude

import           Text.Megaparsec (Dec, ErrorComponent (..), ShowErrorComponent (..))
import qualified Text.Megaparsec as Mega
import           Text.Megaparsec.Prim (MonadParsec)

data ParserError =
    ParserDefault !Dec
    deriving (Eq, Ord, Show)

type Parser s m =
  (MonadParsec ParserError s m, Mega.Token s ~ Positioned Token)

renderParserError :: ParserError -> Text
renderParserError = \case
  ParserDefault dec ->
    T.pack $ showErrorComponent dec

instance ErrorComponent ParserError where
  representFail =
    ParserDefault . representFail
  representIndentation old ref actual =
    ParserDefault $ representIndentation old ref actual

instance ShowErrorComponent ParserError where
  showErrorComponent =
    T.unpack . renderParserError
