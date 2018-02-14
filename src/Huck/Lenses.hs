{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Huck.Lenses (
    _TString
  , _TInteger
  , _TFloat
  , _TBoolean
  , _TDatetime
  , _TArray
  , _TTable

  , key
  ) where

import           Control.Lens (Prism', prism, Traversal', ix)

import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           Data.Time (UTCTime, TimeZone)
import           Data.Vector (Vector)
import           Huck.Data
import           Huck.Position
import           Huck.Prelude

_TString :: Prism' (Toml Position) Text
_TString = prism (TString emptyPosition) $ \n -> case n of
    TString _ v -> pure v
    _ -> Left n

_TInteger :: Prism' (Toml Position) Int64
_TInteger = prism (TInteger emptyPosition) $ \n -> case n of
    TInteger _ v -> pure v
    _ -> Left n

_TFloat :: Prism' (Toml Position) Double
_TFloat = prism (TFloat emptyPosition) $ \n -> case n of
    TFloat _ v -> pure v
    _ -> Left n

_TBoolean :: Prism' (Toml Position) Bool
_TBoolean = prism (TBoolean emptyPosition) $ \n -> case n of
    TBoolean _ v -> pure v
    _ -> Left n

_TDatetime :: Prism' (Toml Position) (UTCTime, TimeZone)
_TDatetime = prism (uncurry (TDatetime emptyPosition)) $ \n -> case n of
    TDatetime _ utc tz -> pure (utc, tz)
    _ -> Left n

_TArray :: Prism' (Toml Position) (Vector (Toml Position))
_TArray = prism (TArray emptyPosition) $ \n -> case n of
    TArray _ v -> pure v
    _ -> Left n

_TTable :: Prism' (Toml Position) (HashMap Text (Toml Position))
_TTable = prism (TTable emptyPosition) $ \n -> case n of
    TTable _ v -> pure v
    _ -> Left n

key :: Text -> Traversal' (TomlDocument a) (Toml a)
key = ix
