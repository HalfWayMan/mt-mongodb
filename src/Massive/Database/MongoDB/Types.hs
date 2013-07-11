{-# LANGUAGE UnicodeSyntax, OverloadedStrings                                                                       #-}
-----------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.Types
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Various useful types.
--
-----------------------------------------------------------------------------------------------------------------------

module Massive.Database.MongoDB.Types ( Entity (..)
                                      , Collection (..)
                                      , toList
                                      ) where

import           Prelude.Unicode
import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                           as Aeson
import qualified Data.HashMap.Strict                  as HM
import qualified Data.Text                            as T
import           Text.Printf
import           Text.Read                            (readMaybe)

import           Massive.Database.MongoDB.MongoEntity

-----------------------------------------------------------------------------------------------------------------------

-- | The 'Entity' data type is used to pair a type serialised from the database
-- with it's unique key.
data Entity α = Entity { entityKey ∷ Key α
                       , entityVal ∷ α
                       }

-- | A show instance exist for any  specialisation of 'Entity' to some type @α@
-- that is  both an instance  of 'MongoEntity'  and 'Show'. This  instance will
-- display the entity in the form @key: <show x>@.
instance (MongoEntity α, Show α) ⇒ Show (Entity α) where
  show (Entity key val) =
    show (fromKey key) ++ ": " ++ show val

instance (MongoEntity α, Aeson.ToJSON α) ⇒ Aeson.ToJSON (Entity α) where
  toJSON (Entity key val) =
    let valJ = Aeson.toJSON val
    in case valJ of
         (Aeson.Object obj) → Aeson.Object $ HM.insert "id" (Aeson.toJSON key) obj
         other              → Aeson.object [ "id"    Aeson..= key
                                           , "value" Aeson..= other ]

-----------------------------------------------------------------------------------------------------------------------

-- | A collection of some type. This is represented as a list of 'Entity'.
newtype Collection α = Collection { unCollection ∷ [Entity α] }

-- | Given a collection, convert it to a list of tuples.
toList ∷ (MongoEntity α) ⇒ Collection α → [(Key α, α)]
toList = map (\(Entity key val) → (key, val)) ∘ unCollection

-- |  Much  like the  'Entity'  type,  there exists  a  'Show'  instance for  a
-- 'Collection' of some type @α@ that  is both an instance of the 'MongoEntity'
-- and 'Show' type clases.
instance (MongoEntity α, Show α) ⇒ Show (Collection α) where
  show (Collection parts) = show parts

-- |  Provide a  convenient  means of  serialising a  collection  to JSON.  The
-- collection is serialised as an object, where  each field of the object is an
-- ID and the value is the serialisation of the entity.
instance (MongoEntity α, Aeson.ToJSON α) ⇒ Aeson.ToJSON (Collection α) where
  toJSON (Collection parts) =
    Aeson.object $ map (\e → (T.pack ∘ show ∘ fromKey ∘ entityKey $ e, Aeson.toJSON ∘ entityVal $ e)) parts

-- | Provide a convenient deserialisation of a collection from JSON.
instance (MongoEntity α, Aeson.FromJSON α) ⇒ Aeson.FromJSON (Collection α) where
  parseJSON (Aeson.Object obj) =
    Collection <$> mapM fromPair (HM.toList obj)
    where
      fromPair (key, val) =
        case readMaybe (T.unpack key) of
          Just oid → Entity (toKey oid) <$> Aeson.parseJSON val
          Nothing  → fail $ printf "failed to parse '%s' as an ObjectID" (T.unpack key)
  parseJSON _ = mzero
