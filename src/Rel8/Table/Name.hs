{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language NamedFieldPuns #-}
{-# language ScopedTypeVariables #-}
{-# language TypeApplications #-}
{-# language TypeFamilies #-}
{-# language UndecidableInstances #-}
{-# language ViewPatterns #-}

module Rel8.Table.Name
  ( namesFromLabels
  , namesFromLabelsWith
  , namesFromLabelsWithA
  , namesFromLabelsHashed
  , showLabels
  , showNames
  )
where

-- base
import Data.Foldable ( fold )
import Data.Functor.Const ( Const( Const ), getConst )
import Data.Functor.Identity (runIdentity)
import Data.List.NonEmpty ( NonEmpty((:|)), intersperse, nonEmpty )
import Data.Maybe ( fromMaybe )
import Prelude

-- base64
import qualified Data.Base64.Types as Base64
import qualified Data.ByteString.Base64 as Base64BS

-- bytestring
import qualified Data.ByteString.Char8 as BS

-- cryptohash-sha1
import qualified Crypto.Hash.SHA1 as SHA1

-- rel8
import Rel8.Schema.HTable (htabulateA, hfield, hspecs)
import Rel8.Schema.Name ( Name( Name ) )
import Rel8.Schema.Spec ( Spec(..) )
import Rel8.Table ( Table(..) )

-- semigroupoids
import Data.Functor.Apply (Apply)

-- text
import qualified Data.Text as T


-- | Construct a table in the 'Name' context containing the names of all
-- columns. Nested column names will be combined with @/@.
--
-- See also: 'namesFromLabelsWith'.
namesFromLabels :: Table Name a => a
namesFromLabels = namesFromLabelsWith go
  where
    go = fold . intersperse "/"


-- | Like 'namesFromLabels' but generates a truncated version
-- with a hash suffix when the name would be too large
namesFromLabelsHashed :: Table Name a => Int -> a
namesFromLabelsHashed size = namesFromLabelsWith (hashLabel size)


-- | Map a non-empty list of labels to a short SQL identifier,
-- truncated and with a hash appended if its concatenation would be too large.
hashLabel :: Int -> NonEmpty String -> String
hashLabel size labels =
  if length full <= size
  then full
  -- 29: SHA1 hashes have 28 bytes in base64 encoding
  else take (size - 29) full ++ "_" ++ T.unpack hashString
  where
    full = fold (intersperse "/" labels)
    hashString
      = Base64.extractBase64
      . Base64BS.encodeBase64
      . SHA1.hash
      . BS.pack
      $ full

-- | Construct a table in the 'Name' context containing the names of all
-- columns. The supplied function can be used to transform column names.
--
-- This function can be used to generically derive the columns for a
-- 'TableSchema'. For example,
--
-- @
-- myTableSchema :: TableSchema (MyTable Name)
-- myTableSchema = TableSchema
--   { columns = namesFromLabelsWith last
--   }
-- @
--
-- will construct a 'TableSchema' where each columns names exactly corresponds
-- to the name of the Haskell field.
namesFromLabelsWith :: Table Name a
  => (NonEmpty String -> String) -> a
namesFromLabelsWith = runIdentity . namesFromLabelsWithA . (pure .)


namesFromLabelsWithA :: (Apply f, Table Name a)
  => (NonEmpty String -> f String) -> f a
namesFromLabelsWithA f = fmap fromColumns $ htabulateA $ \field ->
  case hfield hspecs field of
    Spec {labels} -> Name <$> f (renderLabels labels)


showLabels :: forall a. Table (Context a) a => a -> [NonEmpty String]
showLabels _ = getConst $
  htabulateA @(Columns a) $ \field -> case hfield hspecs field of
    Spec {labels} -> Const (pure (renderLabels labels))


showNames :: forall a. Table Name a => a -> NonEmpty String
showNames (toColumns -> names) = getConst $
  htabulateA @(Columns a) $ \field -> case hfield names field of
    Name name -> Const (pure name)


renderLabels :: [String] -> NonEmpty String
renderLabels labels = fromMaybe (pure "anon") (nonEmpty labels )
