{-# LANGUAGE CPP, UnicodeSyntax, OverloadedStrings, TypeFamilies                                                    #-}
{-# LANGUAGE FlexibleContexts                                                                                       #-}
-----------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.MongoEntity
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Provides the 'MongoEntity' type class.
--
-----------------------------------------------------------------------------------------------------------------------

module Massive.Database.MongoDB.MongoEntity ( MongoEntity (..)
                                            , CollectionName
                                            , module Massive.Database.MongoDB.MongoValue
                                            , (.:), (.:?), (.!=), (≕), (=:), (=::)
                                            ) where

import           Prelude.Unicode
import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Error
import qualified Data.Aeson                          as Aeson
import qualified Data.Bson                           as Bson
import           Data.Maybe                          (fromMaybe)
import qualified Data.Text                           as T

#ifdef WITH_MT_SHARED
import           Data.Monoid
import           Massive.Control.Combinators
import           Massive.Data.Serial
#endif

import           Massive.Database.MongoDB.MongoValue

-----------------------------------------------------------------------------------------------------------------------

type CollectionName = T.Text
type FieldName      = T.Text

class (MongoValue (Key α)) ⇒ MongoEntity α where
  data Key      α
  data Filter   α
  data Document α

  -- | Get the name of the collection to which this entity is to be stored.
  collectionName  ∷ α → CollectionName

  -- | Yields the name of the corresponding field in a collection for a
  --   given filter.
  filterFieldName ∷ Filter α → FieldName

  -- | Convert an 'ObjectId' to a 'Key'.
  toKey           ∷ ObjectId → Key α

  -- | Convert a 'Key' to an 'ObjectId'.
  fromKey         ∷ Key α → ObjectId

  -- | Convert a 'Bson.Document' to a 'Document'.
  toDocument      ∷ Bson.Document → Document α

  -- | Convert a 'Document' to a 'Bson.Document'.
  fromDocument    ∷ Document α → Bson.Document

  -- | Encode an object into a 'Document'.
  encodeEntity    ∷ α → Document α

  -- | Decode an object from a 'Document'; possibly failing.
  decodeEntity    ∷ (Applicative μ, Monad μ) ⇒ Document α → ErrorT String μ α

-----------------------------------------------------------------------------------------------------------------------

instance (MongoEntity α) ⇒ MongoValue (Key α) where
  toValue                    = Bson.ObjId ∘ fromKey
  fromValue (Bson.ObjId oid) = pure (toKey oid)
  fromValue v                = expected "ObjectId" v

instance (MongoEntity α) ⇒ Show (Key α) where
  show = show ∘ fromKey

instance (MongoEntity α) ⇒ Read (Key α) where
  readsPrec n = map (first toKey) ∘ readsPrec n

instance (MongoEntity α) ⇒ Aeson.ToJSON (Key α) where
  toJSON = Aeson.toJSON ∘ fromKey

instance (MongoEntity α) ⇒ Aeson.FromJSON (Key α) where
  parseJSON v = toKey <$> Aeson.parseJSON v

-----------------------------------------------------------------------------------------------------------------------

#ifdef WITH_MT_SHARED

instance (MongoEntity α) ⇒ Serial (Key α) where
  put =
    let f (Oid x y) = put x <> put y
    in f ∘ fromKey

  get =
    (toKey ./ Oid) <$> get <*> get

#endif

-----------------------------------------------------------------------------------------------------------------------

(.:) ∷ (Applicative μ, Monad μ, MongoValue α) ⇒ Bson.Document → T.Text → ErrorT String μ α
(.:) = lookupThrow

(.:?) ∷ (Applicative μ, Monad μ, MongoValue α) ⇒ Bson.Document → T.Text → ErrorT String μ (Maybe α)
(.:?) doc name =
  case lookupMaybe doc name of
    Just val → fromValue val
    Nothing  → pure Nothing

(=:) ∷ (MongoValue α) ⇒ T.Text → α → Bson.Field
(=:) name val = (Bson.:=) name (toValue val)

(=::) ∷ T.Text → Bson.Document → Bson.Field
(=::) name doc = (Bson.:=) name (Bson.Doc doc)

(≕) ∷ (MongoValue α) ⇒ T.Text → α → Bson.Field
(≕) = (=:)

(.!=) ∷ (Applicative μ, Monad μ, MongoValue α) ⇒ ErrorT String μ (Maybe α) → α → ErrorT String μ α
(.!=) mVal val =
  fromMaybe val <$> mVal
