{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Huck.Lexer (
    LexError (..)
  , tokenise
  , tokens
  ) where

import qualified Data.Char as C
import           Data.Scientific (scientific, toBoundedRealFloat)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time (UTCTime (..), Day (..), DiffTime )
import           Data.Time.Calendar (fromGregorianValid)
import           Data.Time.LocalTime (TimeZone, utc, utcToLocalTime
                                     , localTimeToUTC, minutesToTimeZone)
import           Huck.Data.Token
import           Huck.Position

import           Huck.Prelude
import           Prelude (toEnum)

import qualified Text.Megaparsec as Mega
import           Text.Megaparsec hiding ((<|>), string, tokens, token, Token)
import           Text.Megaparsec.Text

newtype LexError = LexError { renderLexError :: Text }
  deriving (Eq, Show)

tokenise :: Text -> Either LexError [Positioned Token]
tokenise t = bimap (LexError . T.pack . show) id (Mega.parse tokens "" t)

tokens :: Parser [Positioned Token]
tokens =
  manyTill (skipSpace *> token) eof

token :: Parser (Positioned Token)
token = skipSpace *> commentP <|>
    (boolP <|> Mega.try floatP <|> try dateTimeP <|> integerP
    <|> lbrackP <|> rbrackP <|> lbraceP <|> rbraceP
    <|> commaP <|> equalP <|> dotP
    <|> stringP) <* skipSpace

skipSpace :: Parser ()
skipSpace =
  try (space *> void newline)
  <|> space

commentP :: Parser (Positioned Token)
commentP = withPos
  (char '#' *> Mega.someTill Mega.asciiChar newline >>= pure . COMMENT . T.pack)

boolP :: Parser (Positioned Token)
boolP = withPos (boolean >>= pure . BOOL)

integerP :: Parser (Positioned Token)
integerP = withPos (integer >>= pure . INTEGER)

floatP :: Parser (Positioned Token)
floatP = withPos (float >>= pure . FLOAT)

lbrackP :: Parser (Positioned Token)
lbrackP = withPos (char '[' *> pure LBRACK)

rbrackP :: Parser (Positioned Token)
rbrackP = withPos (char ']' *> pure RBRACK)

lbraceP :: Parser (Positioned Token)
lbraceP = withPos (char '{' *> pure LBRACE)

rbraceP :: Parser (Positioned Token)
rbraceP = withPos (char '}' *> pure RBRACE)

equalP :: Parser (Positioned Token)
equalP = withPos (char '=' *> pure EQUAL)

commaP :: Parser (Positioned Token)
commaP = withPos (char ',' *> pure COMMA)

dotP :: Parser (Positioned Token)
dotP = withPos (string "." *> pure DOT)

stringP :: Parser (Positioned Token)
stringP =
  withPos $
    (basic' <|> multilineBasic' <|> literal' <|> multilineLiteral' <|> raw') >>= pure . STRING

  where
    raw' = RAW <$> basicKey
    basic' = BASIC <$> basic
    multilineBasic' = BASIC_MULTI <$> multilineBasic
    literal' = LITERAL <$> literal
    multilineLiteral' = LITERAL_MULTI <$> multilineLiteral

dateTimeP :: Parser (Positioned Token)
dateTimeP = withPos (datetime >>= pure . DATE)

string :: [Char] -> Parser Text
string = fmap T.pack . Mega.string

withPos :: Parser a -> Parser (Positioned a)
withPos p = do
  start <- fmap srcInfo getPosition
  val <- p
  end <- fmap srcInfo getPosition
  return (val :@ SrcLoc start end)

srcInfo :: SourcePos -> Position
srcInfo pos = Position (unSourceLine . sourceLine $ pos) (unSourceLine . sourceColumn $ pos)

unSourceLine :: Pos -> Int
unSourceLine = fromIntegral . unPos

-- |
-- = Key
--
-- Keys may be either bare or quoted. Bare keys may only contain letters, numbers,
-- underscores, and dashes (A-Za-z0-9_-). Note that bare keys are allowed to be
-- composed of only digits, e.g. 1234, but are always interpreted as strings.
-- Quoted keys follow the exact same rules as either basic strings or literal
-- strings and allow you to use a much broader set of key names. Best practice
-- is to use bare keys except when absolutely necessary.
-- @
--   key = "value"
--   bare_key = "value"
--   bare-key = "value"
--   1234 = "value"
--
--   "127.0.0.1" = "value"
--   "character encoding" = "value"
--   "ʎǝʞ" = "value"
--   'key2' = "value"
--   'quoted "value"' = "value"
-- @
--
-- A bare key must be non-empty, but an empty quoted key is allowed (though discouraged).
-- @
--      = "no key name"  # INVALID
--   "" = "blank"     # VALID but discouraged
--   '' = 'blank'     # VALID but discouraged
--
basicKey :: Parser Text
basicKey =
  fmap T.pack $ some bareKeyP

  where
    bareKeyP = Mega.oneOf $ ['A'..'Z'] <> ['a'..'z'] <> ['0'..'9'] <> ['_', '-']

-- |
-- = Boolean
--
-- Booleans are just the tokens you're used to. Always lowercase.
--
-- <https://github.com/toml-lang/toml/blob/master/versions/en/toml-v0.4.0.md#boolean>
--
boolean  :: Parser Bool
boolean =
  (True <$ string "true") <|> (False <$ string "false")

-- |
-- = Integer
--
-- Integers are whole numbers. Positive numbers may be prefixed with a
-- plus sign. Negative numbers are prefixed with a minus sign.
--
-- @
--   +99
--   42
--   0
--   -17
-- @
--
-- For large numbers, you may use underscores to enhance
-- readability. Each underscore must be surrounded by at least one
-- digit.
--
-- @
--  1_000
--  5_349_221
--  1_2_3_4_5     # valid but inadvisable
-- @
--
-- Leading zeros are not allowed. Hex, octal, and binary forms are not
-- allowed. Values such as "infinity" and "not a number" that cannot
-- be expressed as a series of digits are not allowed.
--
-- 64 bit (signed long) range expected (−9,223,372,036,854,775,808 to
-- 9,223,372,036,854,775,807).
--
-- <https://github.com/toml-lang/toml/blob/master/versions/en/toml-v0.4.0.md#integer>
--
integer :: Parser Int64
integer =
  signed decimal

-- |
-- = Float
--
--  A float consists of an integer part (which may be prefixed with a
--  plus or minus sign) followed by a fractional part and/or an
--  exponent part. If both a fractional part and exponent part are
--  present, the fractional part must precede the exponent part.
--
-- @
--  # fractional
--  +1.0
--  3.1415
--  -0.01
--
--  # exponent
--  5e+22
--  1e6
--  -2E-2
--
--  # both
--  6.626e-34
-- @
--
-- A fractional part is a decimal point followed by one or more
-- digits.
--
-- An exponent part is an E (upper or lower case) followed by an
-- integer part (which may be prefixed with a plus or minus sign).
--
-- Similar to integers, you may use underscores to enhance
-- readability. Each underscore must be surrounded by at least one
-- digit.
--
-- @
--   9_224_617.445_991_228_313
--   1e1_000
-- @
--
-- 64-bit (double) precision expected.
--
-- <https://github.com/toml-lang/toml/blob/master/versions/en/toml-v0.4.0.md#float>
--
float :: Parser Double
float = signed $ do
  whole <- decimal

  let
    fractional = do
      _ <- char '.'
      x <- digits
      e <- fromMaybe 0 <$> optional exponent
      pure (toNum whole 10 x, e - length x)

    exponental = do
      e <- exponent
      pure (whole, e)

  x <- fractional <|> exponental

  case toBoundedRealFloat . uncurry scientific $ x of
    Left _ ->
      let (coeff, expon) = x in
      fail $ mconcat ["Float is out of range: coefficient = ", show coeff, ", exponent = "
              , show expon]
    Right a ->
      pure a

-- |
-- = String (basic)
--
-- There are four ways to express strings: basic, multi-line basic,
-- literal, and multi-line literal. All strings must contain only
-- valid UTF-8 characters.
--
-- This is the 'basic' string.
--
-- Basic strings are surrounded by quotation marks. Any Unicode
-- character may be used except those that must be escaped: quotation
-- mark, backslash, and the control characters (U+0000 to U+001F).
--
-- @
--  "I'm a string. \"You can quote me\". Name\tJos\u00E9\nLocation\tSF."
-- @
--
-- For convenience, some popular characters have a compact escape sequence.
-- @
--   \b         - backspace       (U+0008)
--   \t         - tab             (U+0009)
--   \n         - linefeed        (U+000A)
--   \f         - form feed       (U+000C)
--   \r         - carriage return (U+000D)
--   \"         - quote           (U+0022)
--   \\         - backslash       (U+005C)
--   \uXXXX     - unicode         (U+XXXX)
--   \UXXXXXXXX - unicode         (U+XXXXXXXX)
-- @
--
-- Any Unicode character may be escaped with the \uXXXX or \UXXXXXXXX
-- forms. The escape codes must be valid Unicode scalar values.
--
-- All other escape sequences not listed above are reserved and, if
-- used, TOML should produce an error.
--
-- <https://github.com/toml-lang/toml/blob/master/versions/en/toml-v0.4.0.md#string>
--
basic :: Parser Text
basic =
  fmap T.pack $
    char '"' *> manyTill tomlchar (char '"')

-- |
-- = String (multi-line basic)
--
-- There are four ways to express strings: basic, multi-line basic,
-- literal, and multi-line literal. All strings must contain only
-- valid UTF-8 characters.
--
-- This is the 'multi-line basic' string.
--
-- Sometimes you need to express passages of text (e.g. translation
-- files) or would like to break up a very long string into multiple
-- lines. TOML makes this easy. Multi-line basic strings are
-- surrounded by three quotation marks on each side and allow
-- newlines. A newline immediately following the opening delimiter
-- will be trimmed. All other whitespace and newline characters remain
-- intact.
--
-- @
--   key1 = """
--   Roses are red
--   Violets are blue"""
-- @
--
-- TOML parsers should feel free to normalize newline to whatever
-- makes sense for their platform.
--
-- @
--   # On a Unix system, the above multi-line string will most likely be the same as:
--   key2 = "Roses are red\nViolets are blue"
--
--   # On a Windows system, it will most likely be equivalent to:
--   key3 = "Roses are red\r\nViolets are blue"
-- @
--
-- For writing long strings without introducing extraneous whitespace,
-- end a line with a \. The \ will be trimmed along with all
-- whitespace (including newlines) up to the next non-whitespace
-- character or closing delimiter. If the first characters after the
-- opening delimiter are a backslash and a newline, then they will
-- both be trimmed along with all whitespace and newlines up to the
-- next non-whitespace character or closing delimiter. All of the
-- escape sequences that are valid for basic strings are also valid
-- for multi-line basic strings.
--
-- @
--   # The following strings are byte-for-byte equivalent:
--   key1 = "The quick brown fox jumps over the lazy dog."
--
--   key2 = """
--   The quick brown \
--
--
--     fox jumps over \
--       the lazy dog."""
--
--       key3 = """\
--          The quick brown \
--          fox jumps over \
--          the lazy dog.\
--          """
-- @
--
-- Any Unicode character may be used except those that must be
-- escaped: backslash and the control characters (U+0000 to
-- U+001F). Quotation marks need not be escaped unless their presence
-- would create a premature closing delimiter.
--
multilineBasic :: Parser Text
multilineBasic =
  let
    triple = T.unpack <$> string "\"\"\""
  in
    multiline (dropContinuation triple) (triple) $
      dropContinuation tomlchar <|> dropContinuation linechar

-- |
-- = String (literal)
--
-- There are four ways to express strings: basic, multi-line basic,
-- literal, and multi-line literal. All strings must contain only
-- valid UTF-8 characters.
--
-- This is the 'literal' string.
--
-- Any Unicode character may be used except those that must be
-- escaped: backslash and the control characters (U+0000 to
-- U+001F). Quotation marks need not be escaped unless their presence
-- would create a premature closing delimiter.
--
-- If you're a frequent specifier of Windows paths or regular
-- expressions, then having to escape backslashes quickly becomes
-- tedious and error prone. To help, TOML supports literal strings
-- where there is no escaping allowed at all. Literal strings are
-- surrounded by single quotes. Like basic strings, they must appear
-- on a single line:
--
-- @
--   # What you see is what you get.
--   winpath  = 'C:\Users\nodejs\templates'
--   winpath2 = '\\ServerX\admin$\system32\'
--   quoted   = 'Tom "Dubs" Preston-Werner'
--   regex    = '<\i\c*\s*>'
-- @
--
-- <https://github.com/toml-lang/toml/blob/master/versions/en/toml-v0.4.0.md#string>
--
literal :: Parser Text
literal =
  fmap T.pack $
    char '\'' *> manyTill (satisfy (not . flip elem ("'\n\r" :: [Char]))) (char '\'')

-- |
-- = String (multi-line literal)
--
-- There are four ways to express strings: basic, multi-line basic,
-- literal, and multi-line literal. All strings must contain only
-- valid UTF-8 characters.
--
-- This is the 'multi-line literal' string.
--
--
-- Since there is no escaping, there is no way to write a single quote
-- inside a literal string enclosed by single quotes. Luckily, TOML
-- supports a multi-line version of literal strings that solves this
-- problem. Multi-line literal strings are surrounded by three single
-- quotes on each side and allow newlines. Like literal strings, there
-- is no escaping whatsoever. A newline immediately following the
-- opening delimiter will be trimmed. All other content between the
-- delimiters is interpreted as-is without modification.
--
-- @
--
--   regex2 = '''I [dw]on't need \d{2} apples'''
--   lines  = '''
--   The first newline is
--   trimmed in raw strings.
--      All other whitespace
--      is preserved.
--   '''
-- @
--
-- For binary data it is recommended that you use Base64 or another
-- suitable ASCII or UTF-8 encoding. The handling of that encoding
-- will be application specific.
--
-- <https://github.com/toml-lang/toml/blob/master/versions/en/toml-v0.4.0.md#string>
--
multilineLiteral :: Parser Text
multilineLiteral =
  let triple = T.unpack <$> string "'''" in multiline triple triple anyChar



-- |
-- = Datetime
--
-- Datetimes are RFC 3339 dates.
--
-- <https://tools.ietf.org/html/rfc3339>
--
-- <https://github.com/toml-lang/toml/blob/master/versions/en/toml-v0.4.0.md#datetime>
datetime :: Parser (UTCTime, TimeZone)
datetime = do
  day <- date
  _ <- char 'T'
  timeofday <- time
  -- FIX validate out of bounds h m
  tz <- choice [
      utc <$ char 'Z'
    , do
        x <- signage
        -- FIX validate out of bounds h m
        xh <- decimalWith $ digitsN 2
        _ <- char ':'
        xm <- decimalWith $ digitsN 2
        pure $ minutesToTimeZone (x $ xh * 60 + xm)
    ]
  let warp x = localTimeToUTC tz $ utcToLocalTime utc x
  pure (warp $ UTCTime day timeofday, tz)

date :: Parser Day
date =  do
  y <- decimalWith $ digitsN 4
  _ <- char '-'
  m <- decimalWith $ digitsN 2
  _ <- char '-'
  d <- decimalWith $ digitsN 2
  case fromGregorianValid y m d of
    Nothing ->
      fail $ mconcat ["Invalid year, month, day: ", show y, show m, show d]
    Just day ->
      pure day

time :: Parser DiffTime
time = do
  (hh :: Integer) <- decimalWith $ digitsN 2
  _ <- char ':'
  (mm :: Integer) <- decimalWith $ digitsN 2
  _ <- char ':'
  (ss :: Integer) <- decimalWith $ digitsN 2
  ms <- optional $ do
    _ <- char '.'
    ds <- some digitChar
    pure $ scientific (toNum (fromIntegral ss) 10 ds) (0 - length ds)
  when (hh > 23 || mm > 59 || ss > 59) $
    fail $ mconcat ["Invalid hour, minute, second: ", show hh, show mm, show ss]
  pure
    (fromIntegral (hh * 60 * 60 + mm * 60) + maybe (fromIntegral ss) (fromRational . toRational) ms)

exponent :: Num a => Parser a
exponent = do
  _ <- char' 'e'
  signed decimal

decimal :: Num a => Parser a
decimal =
  decimalWith digits

decimalWith :: Num a => Parser [Char] -> Parser a
decimalWith p =
  toNum 0 10 <$> p

digits :: Parser [Char]
digits =
  join <$> sepBy1 (some digitChar) (char '_')

digitsN :: Int -> Parser [Char]
digitsN n =
  replicateM n digitChar

toNum :: Num a => a -> a -> [Char] -> a
toNum acc base cs =
  case cs of
    [] ->
      acc
    x : xs ->
      toNum (acc * base + fromIntegral (C.digitToInt x)) base xs

signage :: Num a => Parser (a -> a)
signage = do
  sign <- optional $ char '+' <|> char '-'
  pure $ if sign == Just '-' then negate else id

signed :: Num a => Parser a -> Parser a
signed p = do
  sign <- optional $ char '+' <|> char '-'
  n <- p
  pure $ if sign == Just '-' then negate n else n

tomlchar :: Parser Char
tomlchar =
  choice [
      '\b' <$ try (string "\\b")
    , '\t' <$ try (string "\\t")
    , '\n' <$ try (string "\\n")
    , '\f' <$ try (string "\\f")
    , '\r' <$ try (string "\\r")
    , '"' <$ try (string "\\\"")
    , '\\' <$ try (string "\\\\")
    , '\t' <$ try (string "\t")
    , ' ' <$ try (string " ")
    , try $ string "\\u" >> ((toEnum . toNum 0 16) <$> replicateM 4 hexDigitChar)
    , try $ string "\\U" >> ((toEnum . toNum 0 16) <$> replicateM 8 hexDigitChar)
    , try $ satisfy (\c -> c > '\x001f' && c /= '"' && c /= '\\')
    ]

multiline :: Parser [Char] -> Parser [Char] -> Parser Char -> Parser Text
multiline open close chars =
  strip <$> (fmap T.pack $ open *> manyTill chars close)

strip :: Text -> Text
strip t =
  if "\n" `T.isPrefixOf` t then T.drop 1 t else t

dropContinuation :: Parser a -> Parser a
dropContinuation p =
  p <* continuation

continuation :: Parser ()
continuation =
  choice [
      try $ char '\\' >> eol >> skipMany (choice [void tab, void spaceChar, void eol])
    , pure ()
    ]

linechar :: Parser Char
linechar =
  choice [
      '\n' <$ try (string "\n")
    , '\r' <$ try (string "\r")
    ]
