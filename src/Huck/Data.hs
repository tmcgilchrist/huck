{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
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
  , stripPosition
  ) where

import           Control.Lens

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
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Toml a =
    TString a !Text
  | TInteger a !Int64
  | TFloat a !Double
  | TBoolean a !Bool
  | TDatetime a !UTCTime !TimeZone
  | TArray a !(Vector (Toml a))
  | TTable a !(HashMap Text (Toml a))
  | TComment a !Text
    deriving (Eq, Show, Functor, Foldable, Traversable)

type instance Index (TomlDocument a) = Text

type instance IxValue (TomlDocument a) = (Toml a)
instance Ixed (TomlDocument a) where
  ix k f m = TomlDocument <$> ix k f (tomlDocument m)

emptyTomlDocument :: TomlDocument a
emptyTomlDocument =
  TomlDocument mempty

stripPositions :: TomlDocument Position -> TomlDocument ()
stripPositions = fmap (const ())

stripPosition :: Toml Position -> Toml ()
stripPosition = fmap (const ())
