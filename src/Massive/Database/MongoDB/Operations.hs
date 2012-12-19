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
                                           , update
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

insert ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒
         CollectionName → α → MongoDB.Action μ (Key α)
insert collection entity = do
  (Bson.ObjId objId) ← MongoDB.insert collection (toDocument entity)
  return $! toKey objId
  
------------------------------------------------------------------------------- 

findAll ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
          CollectionName → MongoDB.Action μ (Collection α)
findAll collection = select collection []

-------------------------------------------------------------------------------

select ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
         CollectionName → MongoDB.Document → MongoDB.Action μ (Collection α)
select collection options = do
  docs ← MongoDB.rest =<< MongoDB.find (MongoDB.select options collection)
  Collection <$> mapM fetchKeyValue docs

-------------------------------------------------------------------------------

selectOne ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
            CollectionName → MongoDB.Document → MongoDB.Action μ (Maybe (Entity α))
selectOne collection options = do
  (Collection docs) ← select collection options
  case docs of
    (doc : _) → pure (Just doc)
    _         → pure Nothing

------------------------------------------------------------------------------- 

count ∷ (Functor μ, Applicative μ, MonadIO μ) ⇒
        CollectionName → MongoDB.Document → MongoDB.Action μ Int
count collection options =
  MongoDB.count (MongoDB.select options collection)

-------------------------------------------------------------------------------

update ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒
         CollectionName → Key α → α → MongoDB.Action μ ()
update collection key entity =
  MongoDB.modify (MongoDB.select ["_id" MongoDB.=: (fromKey key)] collection)
                 (toDocument entity)

-------------------------------------------------------------------------------

get ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒
      CollectionName → Key α → MongoDB.Action μ (Maybe α)
get collection key = do
  result ← MongoDB.findOne (MongoDB.select ["_id" MongoDB.=: fromKey key] collection)
  maybe (return Nothing) ((either fail (return ∘ Just) =<<) ∘ runErrorT ∘ fromDocument) result

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
