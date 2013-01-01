-- -*- mode: Haskell; fill-column: 79; default-justification: left;         -*-
{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleContexts             #-}
-------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.Operations
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Database operations.
--
-------------------------------------------------------------------------------

module Massive.Database.MongoDB.Operations ( insert
                                           , findAll
                                           , save
                                           , get
                                           , select
                                           , selectOne
                                           , count
                                           ) where

import           Prelude.Unicode
import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Trans.Control
import qualified Data.Bson                            as Bson
import qualified Database.MongoDB                     as MongoDB

import           Massive.Database.MongoDB.MongoEntity
import           Massive.Database.MongoDB.Types

-------------------------------------------------------------------------------

-- | Insert an entity into the specified collection. This function yields the
-- key for the new document.
insert ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒
         CollectionName → α → MongoDB.Action μ (Key α)
insert collection entity = do
  (Bson.ObjId objId) ← MongoDB.insert collection (toDocument entity)
  return $! toKey objId
  
------------------------------------------------------------------------------- 

-- | Given the name of a collection, yield all the documents.
findAll ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
          CollectionName → MongoDB.Action μ (Collection α)
findAll collection = select collection []

-------------------------------------------------------------------------------

-- | Given a collection and MongoDB query, yield a collection of all the
-- documents that match the query.
select ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
         CollectionName → MongoDB.Document → MongoDB.Action μ (Collection α)
select collection options = do
  docs ← MongoDB.rest =<< MongoDB.find (MongoDB.select options collection)
  Collection <$> mapM fetchKeyValue docs

-------------------------------------------------------------------------------

-- | Similar to 'select', except will only yield zero or one result.
selectOne ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
            CollectionName → MongoDB.Document → MongoDB.Action μ (Maybe (Entity α))
selectOne collection options = do
  (Collection docs) ← select collection options
  case docs of
    (doc : _) → pure (Just doc)
    _         → pure Nothing

------------------------------------------------------------------------------- 

-- | Count the number of documents in the collection that match the specified
-- query.
count ∷ (Functor μ, Applicative μ, MonadIO μ) ⇒
        CollectionName → MongoDB.Document → MongoDB.Action μ Int
count collection options =
  MongoDB.count (MongoDB.select options collection)

-------------------------------------------------------------------------------

-- | Save the specified entity back into the specified collection with the
-- specified key. If a document already eixsts in the collection with the
-- specified key then its contents will be overwritten; otherwise a new
-- document will be created with the specified ID.
save ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒
         CollectionName → Key α → α → MongoDB.Action μ ()
save collection key entity =
  MongoDB.save collection (("_id" MongoDB.=: (fromKey key)) : (toDocument entity))

-------------------------------------------------------------------------------

-- | Get a document from a collection with a given ID. If a document with the
-- given ID cannot be found in the indicated collection, the function yields
-- 'Nothing'.
get ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒
      CollectionName → Key α → MongoDB.Action μ (Maybe α)
get collection key = do
  result ← MongoDB.findOne (MongoDB.select ["_id" MongoDB.=: fromKey key] collection)
  maybe (return Nothing)
        ((either fail (return ∘ Just) =<<) ∘ runErrorT ∘ fromDocument) result

-------------------------------------------------------------------------------

fetchKeyValue ∷ (Applicative μ, Monad μ, MongoEntity α) ⇒ MongoDB.Document → μ (Entity α)
fetchKeyValue doc = do
  case Bson.look "_id" doc of
    Just i →
      case i of
        Bson.ObjId objId → do
          eObj ← runErrorT (fromDocument doc)
          case eObj of
            Left  msg → fail msg
            Right obj → pure $ Entity { entityKey = toKey objId
                                      , entityVal = obj
                                      }
        _ → fail "Expected '_id' field to be an 'ObjectId' in result of selection"
    Nothing →
      fail "Expected to find an '_id' field in the result of selection"
