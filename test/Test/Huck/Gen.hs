{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Test.Huck.Gen (
    genLexableText
  , genLexableTexts
  , genParsableText

  , genToml
  , genTokens
  , genToken
  ) where

import qualified Data.List.NonEmpty as NE
import           Data.Text (Text)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Hedgehog.Corpus

import           Huck
import           Huck.Prelude

import           Prelude ((^^), round, (/), (^), maxBound)

genParsableText :: Gen Text
genParsableText =
  Gen.element [
    "a = 1 \nb = 2.2 \nc = \"some-value\" \nname = \"Lance Uppercut\" \ndob = 1979-05-27T07:32:00-08:00"
  , "[header]\n a = 1"
  ]
genLexableText :: Gen Text
genLexableText =
  Gen.element
    [ -- float
      "3.1315"
    , "+1.0"
    , "5e+22"
    , "-2E-2"
    , "6.626e-34"

    -- ints
    , "1234"
    , "1_000"

     -- booleans
    , "true"
    , "false"

    -- keys
    , "bare-key"
    , "\"ʎǝʞ\""
    , "'quoted value'"
    , "'key.thing'"

    -- string basic
    ,"\"I'm a string. \\\"You can quote me\\\". Name\\tJos\\u00E9\\nLocation\tSF.\""

    -- string multiline basic
    , "\"\"\"\\nRoses are red\\nViolets are blue\\n\"\"\""

    -- string literal
    , "'C:\\Users\\nodejs\\templates'"

    -- string multiline literal
    , "'''\\n" <>
       "The first newline is" <> "\\n" <>
       "trimmed in raw strings." <> "\\n" <>
       "   All other whitespace" <> "\\n" <>
       "   is preserved." <> "\\n" <>
       "'''"

    -- datetime
    , "1979-05-27T07:32:00Z"
    , "1979-05-27T00:32:00-07:00"
    , "1979-05-27T00:32:00.999999-07:00"

    -- punctuation
    , "["
    , "]"
    , "{"
    , "}"
    , "."
    , ","
    , "="
    ]

genLexableTexts :: Gen Text
genLexableTexts = do
  ts <- Gen.nonEmpty (Range.linear 1 1024) genLexableText
  pure . mconcat . NE.toList . NE.intersperse " " $ ts

genToml :: Gen (TomlDocument a)
genToml = Gen.choice [
    pure emptyTomlDocument
  ]

genTokens :: Gen [Token]
genTokens = Gen.list (Range.linear 0 100) genToken

genToken :: Gen Token
genToken =
  Gen.choice [
      BOOL <$> Gen.bool
    , INTEGER <$> Gen.int64 (Range.linear 0 maxBound)
    , FLOAT . round6 <$> Gen.double (Range.exponentialFloat 0.0 9223372036854775807.9)
    , STRING . RAW <$> Gen.element muppets
    -- , DATE
    , COMMENT <$> Gen.element agile

    -- Punctuation
    , pure LBRACK
    , pure RBRACK
    , pure LBRACE
    , pure RBRACE
    , pure EQUAL
    , pure COMMA
    , pure DOT
    ]

round6 :: Double -> Double
round6 x =
  fromInteger (round (x * (10 ^ (6 :: Int)))) / 10.0 ^^ (6 :: Int)
