{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Util (
    lex
  , parse
  , parseText
  , printTokens
  ) where

import           Data.Text
import qualified Data.Text as T

import           Huck.Data
import           Huck.Lexer
import           Huck.Parser
import           Huck.Position
import           Huck.Prelude

import           Prelude (String)

import qualified Text.Megaparsec as Mega

lex :: Text -> Either String [Positioned Token]
lex =
  first Mega.parseErrorPretty .
    Mega.parse tokens "qc"

parse :: [Positioned Token] -> Either String (TomlDocument Position)
parse =
  first Mega.parseErrorPretty . Mega.parse parseTomlDocument "qc"

parseText :: Text -> Either String (TomlDocument Position)
parseText code =
  lex code >>= parse

printTokens :: [Token] -> Text
printTokens = T.intercalate " " . fmap renderToken
