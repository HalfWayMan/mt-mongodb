-- -*- mode: Haskell; fill-column: 79; default-justification: left;         -*-
{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TypeFamilies                 #-}
{-# LANGUAGE FlexibleContexts                                               #-}
-------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.MongoEntity
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Provides the 'MongoEntity' type class.
--
-------------------------------------------------------------------------------

module Massive.Database.MongoDB.MongoEntity ( MongoEntity (..)
                                            , CollectionName
                                            , module Massive.Database.MongoDB.MongoValue
                                            , (.:), (.:?), (.!=), (≕), (=:)
                                            ) where

import           Prelude.Unicode
import           Control.Applicative
import           Control.Monad.Error
import qualified Data.Aeson                          as Aeson
import qualified Data.Bson                           as Bson
import           Data.Maybe                          (fromMaybe)
import qualified Data.Text                           as T
--import           Safe
--import           Web.PathPieces

import           Massive.Database.MongoDB.MongoValue

-------------------------------------------------------------------------------

type CollectionName = T.Text

class (Show (Key α), MongoValue (Key α)) ⇒ MongoEntity α where
  data Key α
  
  toKey          ∷ ObjectId → Key α
  fromKey        ∷ Key α → ObjectId
  toDocument     ∷ α → Bson.Document
  fromDocument   ∷ (Applicative μ, Monad μ) ⇒ Bson.Document → ErrorT String μ α

-------------------------------------------------------------------------------

instance (MongoEntity α) ⇒ MongoValue (Key α) where
  toValue                    = Bson.ObjId ∘ fromKey
  fromValue (Bson.ObjId oid) = pure (toKey oid)
  fromValue v                = expected "ObjectId" v

instance (MongoEntity α) ⇒ Aeson.ToJSON (Key α) where
  toJSON = Aeson.toJSON ∘ fromKey

instance (MongoEntity α) ⇒ Aeson.FromJSON (Key α) where
  parseJSON v = toKey <$> Aeson.parseJSON v
{-
instance (MongoEntity α) ⇒ PathPiece (Key α) where
  fromPathPiece = fmap toKey ∘ readMay ∘ T.unpack
  toPathPiece   = T.pack ∘ show ∘ fromKey
-}
-------------------------------------------------------------------------------

(.:) ∷ (Applicative μ, Monad μ, MongoValue α) ⇒ Bson.Document → T.Text → ErrorT String μ α
(.:) = lookupThrow

(.:?) ∷ (Applicative μ, Monad μ, MongoValue α) ⇒ Bson.Document → T.Text → ErrorT String μ (Maybe α)
(.:?) doc name =
  case lookupMaybe doc name of
    Just val → fromValue val
    Nothing  → pure Nothing

(=:) ∷ (MongoValue α) ⇒ T.Text → α → Bson.Field
(=:) name val = (Bson.:=) name (toValue val)

(≕) ∷ (MongoValue α) ⇒ T.Text → α → Bson.Field
(≕) = (=:)

(.!=) ∷ (Applicative μ, Monad μ, MongoValue α) ⇒ ErrorT String μ (Maybe α) → α → ErrorT String μ α
(.!=) mVal val =
  fromMaybe val <$> mVal
