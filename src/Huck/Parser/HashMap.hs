{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Huck.Parser.HashMap (
    insert
  , HashMapError (..)
  , renderHashMapError
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List (intersect)
import           Data.Text (Text)
import qualified Data.Text as T

import           Huck.Data
import           Huck.Position
import           Huck.Prelude

-- | Errors from constructing a HashMap
data HashMapError
  = KeyRedefinitionError [Text] Text -- | Key(s) redefintion error.
  | KeyExistsError Text [Text]       -- | Common redefinition error.
  | KeyMissingError                  -- | Empty key passed to insert

-- | Inserts a table, 'Table', with the namespaced name, '[Text]', (which
-- may be part of a table array) into a 'Table'.
insert :: ([Text], Toml Position)
     -> HashMap Text (Toml Position)
     -> Either HashMapError (HashMap Text (Toml Position))
insert ([], _) _ = Left KeyMissingError
insert ([name], node) ttbl =
    -- In case 'name' is final (a top-level name)
    case HM.lookup name ttbl of
      Nothing ->
        pure $ HM.insert name node ttbl
      Just (TTable _ t) -> case node of
          (TTable p nt) -> case merge t nt of
                  Left ds -> Left $ KeyRedefinitionError ds name
                  Right r ->
                    pure $ HM.insert name (TTable p r) ttbl
          _ -> commonInsertError node [name]
      Just _ -> commonInsertError node [name]
insert (fullName@(name:ns), node) ttbl =
    -- In case 'name' is not final (not a top-level name)
    case HM.lookup name ttbl of
      Nothing -> do
          r <- insert (ns, node) HM.empty
          pure $ HM.insert name (TTable emptyPosition r) ttbl
      Just (TTable p t) -> do
          r <- insert (ns, node) t
          pure $ HM.insert name (TTable p r) ttbl
      Just _ -> commonInsertError node fullName

-- | Merge two tables, resulting in an error when overlapping keys are
-- found ('Left' will contain those keys).  When no overlapping keys are
-- found the result will contain the union of both tables in a 'Right'.
merge :: HashMap Text (Toml Position)
      -> HashMap Text (Toml Position)
      -> Either [Text] (HashMap Text (Toml Position))
merge existing new =
  case HM.keys existing `intersect` HM.keys new of
    [] -> Right $ HM.union existing new
    ds -> Left ds

commonInsertError :: Toml Position -> [Text] -> Either HashMapError a
commonInsertError what name =
  Left $ KeyExistsError w name

  where
    w = case what of (TTable _ _) -> "tables"
                     _          -> "array of tables"

renderHashMapError :: HashMapError -> Text
renderHashMapError = \case
  KeyExistsError what name -> T.concat [
    "Cannot insert ", what, " '", T.intercalate "." name
    , "' as key already exists."
    ]

  KeyRedefinitionError ns name -> T.concat [
      "Cannot redefine key(s) (", T.intercalate ", " ns
    , "), from table named '", name, "'."
    ]
  KeyMissingError  ->
    "FATAL: Cannot call 'insert' without a name."
