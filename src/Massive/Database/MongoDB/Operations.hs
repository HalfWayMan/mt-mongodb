{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleContexts                                                     #-}
-----------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.Operations
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Database operations.
--
-----------------------------------------------------------------------------------------------------------------------

module Massive.Database.MongoDB.Operations ( -- * Insertion
                                             insert
                                           , insertInto
                                           , insertMany
                                           , insertManyInto
                                           , insertWith
                                           , insertIntoWith

                                             -- * Selection
                                           , SelectorOption
                                           , offset
                                           , limit
                                           , orderAsc
                                           , orderDesc
                                           , filters
                                           , updates

                                           , findAll
                                           , findAllIn
                                           , select
                                           , selectFrom
                                           , selectOne
                                           , selectOneFrom

                                             -- * Counting
                                           , count
                                           , countFrom

                                             -- * Update
                                           , save
                                           , saveTo
                                           , update
                                           , updateWhere

                                             -- * Fetch By ID
                                           , get
                                           , getFrom
                                           , getMany
                                           , getManyFrom

                                             -- * Deleting
                                           , delete
                                           , deleteFrom
                                           , deleteWhere
                                           , deleteFromWhere
                                           ) where

import           Prelude.Unicode
import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Trans.Control
import qualified Data.Bson                            as Bson
import qualified Database.MongoDB                     as MongoDB

import           Massive.Database.MongoDB.FilterOps
import           Massive.Database.MongoDB.MongoEntity
import           Massive.Database.MongoDB.Types

-----------------------------------------------------------------------------------------------------------------------

-- | Insert an entity into the specified collection. This function yields the key for the new document.
insert ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ α → MongoDB.Action μ (Key α)
insert entity =
  insertInto (collectionName entity) entity

-- | Insert an entity into a different collection. This function yields the key for the new document.
insertInto ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → α → MongoDB.Action μ (Key α)
insertInto collection entity = do
  (Bson.ObjId key) ← MongoDB.insert collection (fromDocument (encodeEntity entity))
  return $! toKey key

-- | Similar to 'insert', this function inserts multiple entities into the database, yielding a unique key for each
--   new document.
insertMany ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ [α] → MongoDB.Action μ [Key α]
insertMany                 [] = pure []
insertMany entities @ (x : _) =
  insertManyInto (collectionName x) entities

-- | Similar to 'insertInto', this function inserts multiple entities into a different collection, yielding a unique
--   key for each new document.
insertManyInto ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → [α] → MongoDB.Action μ [Key α]
insertManyInto _                [] = pure []
insertManyInto collection entities = do
  res ← MongoDB.insertMany collection (map (fromDocument ∘ encodeEntity) entities)
  forM res $ \x →
    case x of
      Bson.ObjId key → pure $! toKey key
      _              → throwError $ MongoDB.QueryFailure 1000 "Expected ObjectID as result of 'insertMany'"

-- | Similar to 'insert', but allows you to specify the ID with which the new document is to be created.
insertWith ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ Key α → α → MongoDB.Action μ (Key α)
insertWith key entity =
  insertIntoWith (collectionName entity) key entity

-- | Similar to 'insertInto', but allows you to specify the ID with which the new document is to be created.
insertIntoWith ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → Key α → α → MongoDB.Action μ (Key α)
insertIntoWith collection key entity = do
  (Bson.ObjId insKey) ← MongoDB.insert collection
                                       (("_id" =: fromKey key) : (fromDocument (encodeEntity entity)))
  pure $! toKey insKey

-----------------------------------------------------------------------------------------------------------------------

-- | Yield all the documents.
findAll ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒ MongoDB.Action μ (Collection α)
findAll =
  selectFrom (collectionName (dummyFromDocument document)) document []
  where
    document = toDocument []

-- | Given the name of a collection, yield all the documents.
findAllIn ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
          CollectionName → MongoDB.Action μ (Collection α)
findAllIn collection = selectFrom collection (toDocument []) []

-----------------------------------------------------------------------------------------------------------------------

type SelectorOption α = MongoDB.Query → MongoDB.Query

offset ∷ Int → SelectorOption α
offset n q = q { MongoDB.skip = fromIntegral n }

limit ∷ Int → SelectorOption α
limit n q = q { MongoDB.limit = fromIntegral n }

orderAsc, orderDesc ∷ (MongoEntity α, MongoValue β) ⇒ (β → Filter α) → SelectorOption α
orderAsc  f q = q { MongoDB.sort = (filterFieldName (f undefined) =: (  1  ∷ Int)) : MongoDB.sort q }
orderDesc f q = q { MongoDB.sort = (filterFieldName (f undefined) =: ((-1) ∷ Int)) : MongoDB.sort q }


-- | Select all matching documents from the database. This function yields a 'Collection' of the matching entities
--   decoded from each document in the corresponding entity collection.
--
--   The query can be made using either the quasi-quoted mongoDB query format:
--
--   @select [mongo| { userName: #{name}, userPassword: #{password} } |] []@
--
--   Or using the filter operations:
--
--   @select (filters [UserName `eq` name, UserPassword `eq` password]) []@
--
--   /Note/: in the quasi-quotation the names of the fields correspond to the fields stored in the /database/,
--           rather than the fields of the record structure.
--
select ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
         Document α → [SelectorOption α] → MongoDB.Action μ (Collection α)
select document options =
  selectFrom (collectionName (dummyFromDocument document)) document options

-- | Similar to 'select', except allows you to override the collection from which the entities are selected.
selectFrom ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
             CollectionName → Document α → [SelectorOption α] → MongoDB.Action μ (Collection α)
selectFrom collection document options = do
  cursor  ← MongoDB.find (foldr ($) (MongoDB.select (fromDocument document) collection) options)
  objects ← MongoDB.rest cursor
  Collection <$> mapM fetchKeyValue objects

-- | Similar to 'select', except will only yield zero or one result.
selectOne ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
            Document α → [SelectorOption α] → MongoDB.Action μ (Maybe (Entity α))
selectOne document options =
  selectOneFrom (collectionName (dummyFromDocument document)) document options

-- | Similar to 'selectFrom', except will only yield zero or one result.
selectOneFrom ∷ (Applicative μ, MonadIO μ, MonadBaseControl IO μ, MongoEntity α) ⇒
                CollectionName → Document α → [SelectorOption α] → MongoDB.Action μ (Maybe (Entity α))
selectOneFrom collection document options = do
  mResult ← MongoDB.findOne (foldr ($) (MongoDB.select (fromDocument document) collection) options)
  case mResult of
    Just result → Just <$> fetchKeyValue result
    Nothing     → pure Nothing

-----------------------------------------------------------------------------------------------------------------------

-- | Count the number of documents in the collection that match the specified query.
count ∷ (Functor μ, Applicative μ, MonadIO μ, MongoEntity α) ⇒ Document α → MongoDB.Action μ Int
count document =
  countFrom (collectionName (dummyFromDocument document)) document

-- | Similar to 'count', except you can override the collection name.
countFrom ∷ (Functor μ, Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → Document α → MongoDB.Action μ Int
countFrom collection document =
  MongoDB.count (MongoDB.select (fromDocument document) collection)

-----------------------------------------------------------------------------------------------------------------------

-- | Save an entity with the sepcified key into the database. If a document already exists in the collection with
--   the specified key, then its contents will be overwritten; otherwise a new document will be created with the
--   specified ID.
save ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ Key α → α → MongoDB.Action μ ()
save key entity =
  saveTo (collectionName entity) key entity

-- | Similar to 'save', except allows you to override the collection name.
saveTo ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → Key α → α → MongoDB.Action μ ()
saveTo collection key entity =
  MongoDB.save collection (("_id" =: Bson.ObjId (fromKey key)) : (fromDocument (encodeEntity entity)))

-----------------------------------------------------------------------------------------------------------------------

update ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ Key α → Document α → MongoDB.Action μ ()
update key updateDoc =
  MongoDB.modify (MongoDB.select ["_id" =: (fromKey key)] (collectionName (dummyFromKey key))) (fromDocument updateDoc)

updateWhere ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ Document α → Document α → MongoDB.Action μ ()
updateWhere document updateDoc =
  MongoDB.modify (MongoDB.select (fromDocument document) (collectionName (dummyFromDocument document))) (fromDocument updateDoc)

filters ∷ (MongoEntity α) ⇒ [FilterOp α] → Document α
filters = toDocument

updates ∷ (MongoEntity α) ⇒ [UpdateOp α] → Document α
updates = toDocument

-----------------------------------------------------------------------------------------------------------------------

-- | Given a unique key for an entity, fetch that entity from the database and decode it. If no document with the
--   specified ID was found, the function will yield @Nothing@.
get ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ Key α → MongoDB.Action μ (Maybe α)
get key =
  getFrom (collectionName (dummyFromKey key)) key

-- | Similar to 'get', except allows you to override the collection name.
getFrom ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → Key α → MongoDB.Action μ (Maybe α)
getFrom collection key = do
  result ← MongoDB.findOne (MongoDB.select ["_id" =: fromKey key] collection)
  maybe (return Nothing)
        ((either fail (return ∘ Just) =<<) ∘ runErrorT ∘ decodeEntity ∘ toDocument) result

-- | The application of 'get' to many keys.
getMany ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ [Key α] → MongoDB.Action μ [Maybe α]
getMany = mapM get

-- | Similar to 'getMany', except allows you to override the collection name.
getManyFrom  ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → [Key α] → MongoDB.Action μ [Maybe α]
getManyFrom collection = mapM (getFrom collection)

-----------------------------------------------------------------------------------------------------------------------

-- | Delete an entity from the database given it's unique ID.
delete ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ Key α → MongoDB.Action μ ()
delete key =
  deleteFrom (collectionName (dummyFromKey key)) key

-- | Similar to 'delete', except allows you to override the collection name.
deleteFrom ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → Key α → MongoDB.Action μ ()
deleteFrom collection key =
  MongoDB.deleteOne (MongoDB.select ["_id" =: fromKey key] collection)

deleteWhere ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ Document α → MongoDB.Action μ ()
deleteWhere document =
  deleteFromWhere (collectionName (dummyFromDocument document)) document

deleteFromWhere ∷ (Applicative μ, MonadIO μ, MongoEntity α) ⇒ CollectionName → Document α → MongoDB.Action μ ()
deleteFromWhere collection document =
  MongoDB.delete (MongoDB.select (fromDocument document) collection)

-----------------------------------------------------------------------------------------------------------------------

dummyFromKey ∷ Key α → α
dummyFromKey _ = undefined

dummyFromDocument ∷ Document α → α
dummyFromDocument _ = undefined

fetchKeyValue ∷ (Applicative μ, Monad μ, MongoEntity α) ⇒ MongoDB.Document → μ (Entity α)
fetchKeyValue doc = do
  case Bson.look "_id" doc of
    Just i →
      case i of
        Bson.ObjId objId → do
          eObj ← runErrorT (decodeEntity (toDocument doc))
          case eObj of
            Left  msg → fail msg
            Right obj → pure $ Entity { entityKey = toKey objId
                                      , entityVal = obj
                                      }
        _ → fail "Expected '_id' field to be an 'ObjectId' in result of selection"
    Nothing →
      fail "Expected to find an '_id' field in the result of selection"
