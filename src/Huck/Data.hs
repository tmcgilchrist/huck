{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
module Huck.Data (
    TomlDocument (..)
  , Toml (..)
  , emptyTomlDocument

  -- * Tokens
  , X.Tokens (..)
  , X.Token (..)
  , X.renderToken
  , X.STRING (..)
  , X.renderString

  , stripPositions
  ) where

import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           Data.Time (UTCTime, TimeZone)
import           Data.Vector (Vector)

import qualified Huck.Data.Token as X
import           Huck.Position (Position(..))
import           Huck.Prelude

newtype TomlDocument a =
  TomlDocument {
    tomlDocument :: HashMap Text (Toml a)
  } deriving (Eq, Show, Functor)

data Toml a =
    TString a !Text
  | TInteger a !Int64
  | TFloat a !Double
  | TBoolean a !Bool
  | TDatetime a !UTCTime !TimeZone
  | TArray a !(Vector (Toml a))
  | TTable a !(HashMap Text (Toml a))
  | TComment a !Text
    deriving (Eq, Show, Functor)

emptyTomlDocument :: TomlDocument a
emptyTomlDocument =
  TomlDocument mempty

stripPositions :: TomlDocument Position -> TomlDocument ()
stripPositions a = TomlDocument $ fmap (fmap (const ())) (tomlDocument a)
