{-# LANGUAGE NoImplicitPrelude #-}
module Huck.Data (
    TomlDocument (..)
  , Toml (..)
  , emptyTomlDocument
  ) where

import           Data.HashMap.Strict (HashMap)
import           Data.Text
import           Data.Time (UTCTime, TimeZone)
import           Data.Vector (Vector)

import           Huck.Prelude

newtype TomlDocument a =
  TomlDocument {
    tomlDocument :: HashMap Text (Toml a)
  } deriving (Eq, Show)

data Toml a =
    TString a !Text
  | TInteger a !Int64
  | TFloat a !Double
  | TBoolean a !Bool
  | TDatetime a !UTCTime !TimeZone
  | TArray a !(Vector (Toml a))
  | TTable a !(HashMap Text (Toml a))
    deriving (Eq, Show)

emptyTomlDocument :: TomlDocument ()
emptyTomlDocument =
  TomlDocument mempty
