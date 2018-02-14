{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Huck.Parser (
    Parser
  , ParserError (..)
  , renderParserError
  , parseTomlDocument
  , parseToml

  -- | Individual fragment parsers
  , parseTable
  , pLitDate
  ) where

import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime(..), TimeZone)
import qualified Data.Vector as V

import           Huck.Data
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
  _ <- Mega.many pComment
  top <- Mega.many parseKeyValue <* Mega.optional pComment
  t <- Mega.many parseTable <* Mega.eof
  -- TODO combine nested keys correctly.
  --
  pure . TomlDocument $ HM.union (HM.fromList top) (HM.fromList $ (\(x, y) ->  (T.intercalate "." x, y)) <$> t)

parseTable :: Parser s m => m ([Text], Toml Position)
parseTable = do
  header <- parseTableHeader
  keys <- Mega.many parseKeyValue <* Mega.optional pComment
  pure . (,) (snd header) $ TTable (fst header) (HM.fromList keys)

parseTableHeader :: Parser s m => m (Position, [Text])
parseTableHeader =
  Mega.between (pToken LBRACK) (pToken RBRACK) pHeader

parseToml :: Parser s m => m (Toml Position)
parseToml =
  Mega.choice [
      pLitFloat
    , pLitInteger
    , pLitBoolean
    , pLitString
    , pLitDate
    , pLitArray
    ]

parseKeyValue :: Parser s m => m (Text, Toml Position)
parseKeyValue = label "key / value" $ do
  k <- parseKey
  void $ pToken EQUAL
  a <- fmap (k,) parseToml -- <|> parseTable
  _ <- Mega.optional pComment
  pure a

parseKey :: Parser s m => m Text
parseKey = fmap snd pKey

pLitArray :: Parser s m => m (Toml Position)
pLitArray = label "array literal" $ do
  a <-  pToken LBRACK
  h <- pThing
  hs <- Mega.many (pToken COMMA *> pThing)
  _ <- pToken RBRACK
  pure $ TArray a (V.fromList (h:hs))

  where
    pThing = pLitString <|> pLitBoolean <|> pLitInteger <|> pLitFloat <|> pLitArray

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

-- TODO This should be a NonEmpty list here
pHeader :: Parser s m => m (Position, [Text])
pHeader = label "header literal" $ do

  x@(a:_) <- Mega.sepBy1 keyComponent (pToken DOT)

  let b = snd <$> x
  pure (fst a, b)

  where
    keyComponent :: Parser s m => m (Position, Text)
    keyComponent = tryPosToken $ \case
      STRING (RAW key) ->
        Just key
      STRING (BASIC key) ->
        Just key
      _ ->
        Nothing

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

pComment :: Parser s m => m (Position, Text)
pComment =
  label "string literal" .
  tryPosToken $ \case
    COMMENT str ->
      Just str
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
