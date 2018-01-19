{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
module Huck.Parser (
    Parser
  , ParserError (..)
  , renderParserError
  , parseTomlDocument
  , parseToml
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime(..), TimeZone)

import           Huck.Data
import           Huck.Data.Token
import           Huck.Position
import           Huck.Prelude

import           Text.Megaparsec ( Dec, label
                                 , ErrorComponent(..), ShowErrorComponent(..))
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

parseTomlDocument :: Parser s m => m (TomlDocument Position)
parseTomlDocument = do
  -- TODO Add table parsing
  -- top <- Mega.manyTill parseKeyValue _LBRACK
  -- Mega.manyTill parseTable EOF

  TomlDocument . HM.fromList <$> Mega.many parseKeyValue <* Mega.eof

-- parseTable :: Parser s m => m (Toml Position)
-- parseTable = do
--   header <- pToken LBRACK *> p
--   undefined

parseToml :: Parser s m => m (Toml Position)
parseToml =
  Mega.choice [
      pLitFloat
    , pLitInteger
    , pLitBoolean
    , pLitString
    , pLitDate
    ]

parseKeyValue :: Parser s m => m (Text, Toml Position)
parseKeyValue = do
  k <- parseKey
  void $ pToken EQUAL
  v <- parseToml
  pure (k, v)

parseKey :: Parser s m => m Text
parseKey = fmap snd pKey

pLitString :: Parser s m => m (Toml Position)
pLitString = uncurry TString <$> pString

pLitBoolean :: Parser s m => m (Toml Position)
pLitBoolean = uncurry TBoolean <$> pBoolean

pLitInteger :: Parser s m => m (Toml Position)
pLitInteger = uncurry TInteger <$> pInteger

pLitFloat :: Parser s m => m (Toml Position)
pLitFloat = uncurry TFloat <$> pFloat

pLitDate :: Parser s m => m (Toml Position)
pLitDate =
  fmap (\(p, (u, t)) -> TDatetime p u t) pDate

-- pHeader :: Parser s m => m (Position, Text)
-- pHeader =
--   label "header literal" .
--   tryPosToken $ \case
--     STRING (RAW key) ->
--       Just key
--     _ ->
--       Nothing

pKey :: Parser s m => m (Position, Text)
pKey =
  label "key literal" .
  tryPosToken $ \case
    STRING (RAW key) ->
      Just key
    STRING (BASIC key) ->
      Just key
    _ ->
      Nothing

pBoolean :: Parser s m => m (Position, Bool)
pBoolean =
  label "boolean literal" .
  tryPosToken $ \case
    BOOL f -> Just f
    _ -> Nothing

pInteger :: Parser s m => m (Position, Int64)
pInteger =
  label "integer literal" .
  tryPosToken $ \case
    INTEGER f -> Just f
    _ -> Nothing

pFloat :: Parser s m => m (Position, Double)
pFloat =
  label "float literal" .
  tryPosToken $ \case
    FLOAT f -> Just f
    _ -> Nothing

pDate :: Parser s m => m (Position, (UTCTime, TimeZone))
pDate =
  label "date literal" .
  tryPosToken $ \case
    DATE (date, tz) ->
      Just (date, tz)
    _ ->
      Nothing

pString :: Parser s m => m (Position, Text)
pString =
  label "string literal" .
  tryPosToken $ \case
    STRING str ->
      Just . renderString $ str
    _ ->
      Nothing


pToken :: Parser s m => Token -> m Position
pToken tok0 =
  label ("“" <> T.unpack (renderToken tok0) <> "”") .
  tryToken $ \pos tok ->
    if tok0 == tok then
      Just pos
    else
      Nothing

tryToken :: Parser s m => (Position -> Token -> Maybe a) -> m a
tryToken f  =
  tryPositioned $ \(tok :@ SrcLoc start _) ->
    f start tok

tryPosToken :: Parser s m => (Token -> Maybe a) -> m (Position, a)
tryPosToken f  =
  tryPositioned $ \(tok :@ SrcLoc start _) -> (start,) <$> f tok

tryPositioned :: Parser s m => (Positioned Token -> Maybe a) -> m a
tryPositioned f =
  let
    testToken p =
      case f p of
        Nothing ->
          Left (Set.singleton (Mega.Tokens $ p :| []), Set.empty, Set.empty)
        Just x ->
          Right x
  in
    Mega.token testToken Nothing
