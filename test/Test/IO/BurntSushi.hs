{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
module Test.IO.BurntSushi (tests) where

import           Data.Aeson
import qualified Data.ByteString as B
import           Data.ByteString.Lazy (fromStrict)
import           Data.FileEmbed
import qualified Data.HashMap.Strict as M
import           Data.List (isPrefixOf, isSuffixOf)
import           Data.Text (Text)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Vector (Vector)
import qualified Data.Vector as V
import           Hedgehog
import           Huck
import           Huck.Pretty (ppDateTime)
import           Test.Huck.Util

-- * Special BurntSushi ToJSON type class and instances

-- | Type class for conversion to BurntSushi-style JSON.
--
-- BurntSushi has made a language agnostic test suite available that
-- this library uses. This test suit expects that values are encoded
-- as JSON objects with a 'type' and a 'value' member.
class ToBsJSON a where
  toBsJSON :: a -> Value

-- | Provide a 'toBsJSON' instance to the 'VTArray'.
instance (ToBsJSON a) => ToBsJSON (Vector a) where
  toBsJSON = Array . V.map toBsJSON
  {-# INLINE toBsJSON #-}

-- | Provide a 'toBsJSON' instance to the 'TomlDocument'.
instance ToBsJSON (TomlDocument a) where
  toBsJSON = Object . M.map toBsJSON . tomlDocument
  {-# INLINE toBsJSON #-}

-- | Provide a 'toBsJSON' instance to the 'NTable'.
instance (ToBsJSON v) => ToBsJSON (M.HashMap Text v) where
  toBsJSON = Object . M.map toBsJSON
  {-# INLINE toBsJSON #-}

-- | 'ToBsJSON' instances for the 'TValue' type that produce Aeson (JSON)
-- in line with BurntSushi's language agnostic TOML test suite.
--
-- As seen in this function, BurntSushi's JSON encoding explicitly
-- specifies the types of the values.
instance ToBsJSON (Toml a) where
  toBsJSON (TTable _ v)    = toBsJSON v
  toBsJSON (TString _ v)   = object [ "type"  .= toJSON ("string" :: String)
                                  , "value" .= toJSON v ]
  toBsJSON (TInteger _ v)  = object [ "type"  .= toJSON ("integer" :: String)
                                  , "value" .= toJSON (show v) ]
  toBsJSON (TFloat _ v)    = object [ "type"  .= toJSON ("float" :: String)
                                  , "value" .= toJSON (show v) ]
  toBsJSON (TBoolean _ v)  = object [ "type"  .= toJSON ("bool" :: String)
                                  , "value" .= toJSON (if v then "true" else "false" :: String) ]
  toBsJSON (TDatetime _ v1 v2) = object [ "type"  .= toJSON ("datetime" :: String)
                                  , "value" .= toJSON (ppToText $ ppDateTime v2 v1) ]
  toBsJSON (TArray _ v)    = object [ "type"  .= toJSON ("array" :: String)
                                  , "value" .= toBsJSON v ]
  toBsJSON (TComment _ _) = ""

allFiles :: [(FilePath, B.ByteString)]
allFiles = $(makeRelativeToProject "test/data/BurntSushi" >>= embedDir)

validPairs :: [(String, (B.ByteString, B.ByteString))]
validPairs =
    map (\(tFP, tBS) -> (stripExt tFP, (tBS, jsonCounterpart tFP))) tomlFiles
  where
    validFiles = filter (\(f, _) -> "valid" `isPrefixOf` f) allFiles
    filterOnSuffix sfx = filter (\(f, _) -> sfx `isSuffixOf` f)
    tomlFiles = filterOnSuffix ".toml" validFiles
    jsonFiles = filterOnSuffix ".json" validFiles
    stripExt fp = take (length fp - 5) fp
    jsonCounterpart tFP =
      case filter (\(f, _) -> f == stripExt tFP ++ ".json") jsonFiles of
        []       -> error $ "Could not find a JSON counterpart for: " ++ tFP
        [(_, j)] -> j
        _        -> error $ "Expected one, but found several \
                            \JSON counterparts for: " ++ tFP

invalidTomlFiles :: [(FilePath, B.ByteString)]
invalidTomlFiles = filter (\(f, _) -> "invalid" `isPrefixOf` f) allFiles

-- BurntSushi's test suite
--
-- test equality of resulting JSON (valid)
prop_testBurntSushi :: Property
prop_testBurntSushi = property . test $
  mapM_ (\(fp, (tBS, jBS)) -> assertIsValid fp tBS jBS) validPairs
-- test parse failures of malformed TOML files (invalid)

-- prop_testBurntSushi_malformed :: Property
-- prop_testBurntSushi_malformed = property . test $
--   mapM_ (uncurry assertParseFailure) invalidTomlFiles

assertIsValid :: MonadTest m => String -> B.ByteString -> B.ByteString -> m ()
assertIsValid f tomlBS jsonBS =
  case parseText (decodeUtf8 tomlBS) of
    Left e -> footnote ("Could not parse TOML file: " ++ f ++ ".toml\n" ++ show e) >> failure
    Right tomlTry -> case eitherDecode (fromStrict jsonBS) of
      Left _ -> footnote ("Could not parse JSON file: " ++ f ++ ".json") >> failure
      Right jsonCorrect -> footnote ("File " ++ f ++ ".json") >> jsonCorrect === toBsJSON tomlTry

assertParseFailure :: MonadTest m => String -> B.ByteString -> m ()
assertParseFailure f tomlBS =
      case parseText (decodeUtf8 tomlBS) of
        Left _ -> return ()
        Right _ -> footnote ("Parser accepted invalid TOML file: " ++ f) >> failure

return []
tests :: IO Bool
tests =
  checkParallel $$(discover)
