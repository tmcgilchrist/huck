{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Huck.Position (
    SrcInfo (..)
  , Position (..)
  , Positioned (..)

  , emptyPosition
  ) where

import           Huck.Prelude

import           Text.Megaparsec (ShowToken (..), Stream (..))
import           Text.Megaparsec.Pos (SourcePos (..), unsafePos)

-- | A single point in the source file.
data Position = Position {
    posLine :: !Int
  , posColumn :: !Int
  } deriving (Eq, Ord, Show)

-- | A range in the source file.
data SrcInfo
  = SrcLoc !Position !Position
  deriving (Eq, Ord, Show)

-- | A value and character range pair
data Positioned a = !a :@ !SrcInfo
  deriving (Eq, Ord, Show, Functor)

instance ShowToken a => ShowToken (Positioned a) where
  showTokens = showTokens . fmap (\(l :@ _ ) -> l)

instance Ord a => Stream [Positioned a] where
  type Token [Positioned a] = Positioned a

  uncons = \case
    [] ->
      Nothing
    x : xs ->
      Just (x, xs)

  updatePos _ _ _ (_ :@ SrcLoc start end) =
    (toSourcePos start, toSourcePos end)

toSourcePos :: Position -> SourcePos
toSourcePos = \case
  Position line col ->
    SourcePos ""
      (unsafePos $ fromIntegral line)
      (unsafePos $ fromIntegral col)

emptyPosition :: Position
emptyPosition = Position {posLine = 0, posColumn = 0}
