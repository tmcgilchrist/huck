{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Test.Huck.Util (
    lex
  ) where

import           Huck.Data.Token
import           Huck.Lexer
-- import           Huck.NewParser
import           Huck.Position
import           Huck.Prelude
import           Data.Text
import           Prelude (String)
import qualified Text.Megaparsec as Mega

lex :: Text -> Either String [Positioned Token]
lex =
  first Mega.parseErrorPretty .
    Mega.parse tokens "qc"

-- parse :: [Positioned Token] -> Either String (TomlDocument Position)
-- parse =
--   first Mega.parseErrorPretty . Mega.parse parseTomlDocument "qc"
