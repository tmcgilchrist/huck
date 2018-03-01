{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Huck.Parser.HashMap (
    Explicitness (..)
  , isExplicit
  , insert
  ) where

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import           Data.List (intersect)
import           Data.Text (Text)
import qualified Data.Text as T
import           Huck.Data
import           Huck.Position
import           Huck.Prelude

-- | To mark whether or not a 'Table' has been explicitly defined.
-- See: https://github.com/toml-lang/toml/issues/376
data Explicitness = Explicit | Implicit

-- | Convenience function to get a boolean value.
isExplicit :: Explicitness -> Bool
isExplicit Explicit = True
isExplicit Implicit = False

-- | Inserts a table, 'Table', with the namespaced name, '[Text]', (which
-- may be part of a table array) into a 'Table'.
insert :: Explicitness
     -> ([Text], Toml Position)
     -> HashMap Text (Toml Position)
     -> Either Text (HashMap Text (Toml Position))
insert _ ([], _) _ = Left "FATAL: Cannot call 'insert' without a name."
insert _ex ([name], node) ttbl =
    -- In case 'name' is final (a top-level name)
    case HM.lookup name ttbl of
      Nothing -> -- do when (isExplicit ex) $ updateExState [name] node
        pure $ HM.insert name node ttbl
      Just (TTable _ t) -> case node of
          (TTable p nt) -> case merge t nt of
                  Left ds -> nameInsertError ds name
                  Right r -> -- do when (isExplicit ex) $ updateExStateOrError [name] node
                    pure $ HM.insert name (TTable p r) ttbl
          _ -> commonInsertError node [name]
      Just _ -> commonInsertError node [name]
insert _ex (fullName@(name:ns), node) ttbl =
    -- In case 'name' is not final (not a top-level name)
    case HM.lookup name ttbl of
      Nothing -> do
          r <- insert Implicit (ns, node) HM.empty
          -- when (isExplicit ex) $ updateExState fullName node
          pure $ HM.insert name (TTable emptyPosition r) ttbl
      Just (TTable p t) -> do
          r <- insert Implicit (ns, node) t
          -- when (isExplicit ex) $ updateExStateOrError fullName node
          pure $ HM.insert name (TTable p r) ttbl
      Just _ -> commonInsertError node fullName


-- | Merge two tables, resulting in an error when overlapping keys are
-- found ('Left' will contain those keys).  When no overlapping keys are
-- found the result will contain the union of both tables in a 'Right'.
merge :: HashMap Text (Toml Position) -> HashMap Text (Toml Position) -> Either [Text] (HashMap Text (Toml Position))
merge existing new =
  case HM.keys existing `intersect` HM.keys new of
    [] -> Right $ HM.union existing new
    ds -> Left ds

-- TOML tables maybe redefined when first definition was implicit.
-- For instance a top-level table `a` can implicitly defined by defining a non top-level
-- table `b` under it (namely with `[a.b]`). Once the table `a` is subsequently defined
-- explicitly (namely with `[a]`), it is then not possible to (re-)define it again.
-- A parser state of all explicitly defined tables is maintained, which allows
-- raising errors for illegal redefinitions of such.


-- updateExStateOrError :: [Text] -> Toml Position -> S.StateT (Set [Text]) m (Either Text ())
-- updateExStateOrError name node@(TTable _ _) = do
--     explicitlyDefinedNames <- S.get
--     when (Set.member name explicitlyDefinedNames) $ tableClashError name
--     updateExState name node
-- updateExStateOrError _ _ = return ()

-- | HashMap redefinition error.
-- tableClashError :: [Text] -> Either Text a
-- tableClashError name = Left $ T.concat
--     [ "Cannot redefine table ('", T.intercalate ", " name , "'." ]

-- | Like 'updateExStateOrError' but does not raise errors. Only use this when sure
-- that redefinitions cannot occur.
-- updateExState :: Parser s m => [Text] -> Toml Position -> S.StateT (Set [Text]) m ()
-- updateExState name (TTable _ _) = S.modify' (Set.insert name)
-- updateExState _ _ = pure ()
-- updateExState name (TTable _ _) = S.modify' (Set.insert name) >> pure ()
-- updateExState _ _ = pure ()

-- * Parse errors resulting from invalid TOML

-- | Key(s) redefintion error.
nameInsertError :: [Text] -> Text -> Either Text a
nameInsertError ns name = Left $ T.concat
    [ "Cannot redefine key(s) (", T.intercalate ", " ns
    , "), from table named '", name, "'." ]


-- | Common redefinition error.
commonInsertError :: Toml Position -> [Text] -> Either Text a
commonInsertError what name = Left $ T.concat
    [ "Cannot insert ", w, " '", n, "' as key already exists." ]
  where
    n = T.intercalate "." name
    w = case what of (TTable _ _) -> "tables"
                     _          -> "array of tables"
