{-# LANGUAGE UnicodeSyntax, OverloadedStrings, FlexibleContexts                                                     #-}
-----------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.FilterOps
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Filtering operations for building queries.
--
-----------------------------------------------------------------------------------------------------------------------

module Massive.Database.MongoDB.FilterOps ( -- * Filtering Operations
                                            FilterOp
                                          , eq, neq, lt, lte, gt, gte
                                          , or, ors
                                          , hasElem

                                            -- * Update Operations
                                          , UpdateOp
                                          , set
                                          , inc, dec
                                          , pop, push, pull, pushAll, pullAll
                                          , addToSet, addManyToSet
                                          , popMany
                                          ) where

import           Prelude         hiding (or)
import qualified Data.Bson       as Bson
import           Massive.Database.MongoDB.MongoValue
import           Massive.Database.MongoDB.MongoEntity

-----------------------------------------------------------------------------------------------------------------------

type FilterOp α = Bson.Field

stdFilterDef ∷ (MongoEntity α, MongoValue β) ⇒ Bson.Label → (β → Filter α) → β → FilterOp α
stdFilterDef op f v =
  filterFieldName (f undefined) =: Bson.Doc [op =: toValue v]

eq, neq, lt, lte, gt, gte ∷ (MongoEntity α, MongoValue β) ⇒ (β → Filter α) → β → FilterOp α
eq f v = filterFieldName (f undefined) =: toValue v
neq    = stdFilterDef "$ne"
lt     = stdFilterDef "$lt"
lte    = stdFilterDef "$lte"
gt     = stdFilterDef "$gt"
gte    = stdFilterDef "$gte"

hasElem :: (MongoEntity a, MongoValue b) => ([b] -> Filter a) -> b -> FilterOp a
hasElem f v = filterFieldName (f undefined) =: toValue v

or ∷ FilterOp α → FilterOp α → FilterOp α
or x y = "$or" =: Bson.Doc [x, y]

ors ∷ [FilterOp α] → FilterOp α
ors fs = "$or" =: Bson.Doc fs

-----------------------------------------------------------------------------------------------------------------------

type UpdateOp α = Bson.Field

set ∷ (MongoEntity α, MongoValue β) ⇒ (β → Filter α) → β → UpdateOp α
set f v = "$set" =: Bson.Doc [filterFieldName (f v) =: toValue v]

inc, dec ∷ (MongoEntity α, Num β, MongoValue β) ⇒ (β → Filter α) → β → UpdateOp α
inc f v = "$inc" =: Bson.Doc [filterFieldName (f v) =: toValue v]
dec f v = "$dec" =: Bson.Doc [filterFieldName (f v) =: toValue v]

push, addToSet, pull ∷ (MongoEntity α, MongoValue β) ⇒ ([β] → Filter α) → β → UpdateOp α
push     f v = "$push"     =: Bson.Doc [filterFieldName (f       [v]) =: toValue v]
addToSet f v = "$addToSet" =: Bson.Doc [filterFieldName (f undefined) =: toValue v]
pull     f v = "$pull"     =: Bson.Doc [filterFieldName (f undefined) =: toValue v]

pushAll, pullAll, addManyToSet ∷ (MongoEntity α, MongoValue β) ⇒ ([β] → Filter α) → [β] → UpdateOp α
pushAll      f v = "$pushAll"      =: Bson.Doc [filterFieldName (f undefined) =:                      Bson.Array (map toValue v) ]
pullAll      f v = "$pullAll"      =: Bson.Doc [filterFieldName (f undefined) =:                      Bson.Array (map toValue v) ]
addManyToSet f v = "$addManyToSet" =: Bson.Doc [filterFieldName (f undefined) =: Bson.Doc ["$each" =: Bson.Array (map toValue v)]]

pop ∷ (MongoEntity α, MongoValue β) ⇒ ([β] → Filter α) → UpdateOp α
pop f = "$pop" =: Bson.Doc [filterFieldName (f undefined) =: Bson.Int32 1]

popMany ∷ (MongoEntity α, MongoValue β) ⇒ ([β] → Filter α) → Int → UpdateOp α
popMany f i = "$pop" =: Bson.Doc [filterFieldName (f undefined) =: Bson.Int32 (fromIntegral i)]
