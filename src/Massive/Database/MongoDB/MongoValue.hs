{-# LANGUAGE CPP, UnicodeSyntax, OverloadedStrings, TupleSections                                                   #-}
{-# LANGUAGE FlexibleInstances, TemplateHaskell                                                                     #-}
{-# OPTIONS_GHC -fno-warn-orphans                                                                                   #-}
-----------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.MongoValue
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Marshall values to and from MongoDB.
--
-----------------------------------------------------------------------------------------------------------------------

module Massive.Database.MongoDB.MongoValue ( MongoValue (..)
                                           , Bson.Value (..)
                                           , Bson.ObjectId (..)
                                           , nullObjectId
                                           , newObjectId
                                           , expected
                                           , lookupMaybe
                                           , lookupThrow
                                           ) where

import           Prelude.Unicode
import           Control.Applicative
import           Control.Arrow              ((&&&))
import           Control.Monad.Error
import qualified Data.Aeson                 as Aeson
import           Data.Bson                  (ObjectId (..))
import qualified Data.Bson                  as Bson
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as LBS
import           Data.Int
import           Data.List                  (find)
import qualified Data.Map                   as M
import           Data.Ratio
import           Data.String
import qualified Data.Text                  as T
import qualified Data.Text.Lazy             as LT
import           Data.Time
import           Data.Word
import           Text.Read                  (readMaybe)
import           Text.Printf

#ifdef WITH_MT_SHARED
import qualified Data.ByteString.Base64.URL as Base64U
import qualified Data.Text.Encoding         as TE
import           Massive.Debug
import           Massive.Data.Decimal
import           Massive.Data.Money
import           Massive.Data.NEList
import           Massive.Data.Password
#endif

-----------------------------------------------------------------------------------------------------------------------

-- | Any type that can be converted to a MongoDB value should be an instance of
-- this type class.
class MongoValue α where
  toValue   ∷ α → Bson.Value
  fromValue ∷ (Applicative μ, Monad μ) ⇒ Bson.Value → ErrorT String μ α

-----------------------------------------------------------------------------------------------------------------------

instance IsString Bson.Value where
  fromString = Bson.String ∘ T.pack

instance MongoValue Bson.Value where
  toValue   = id
  fromValue = pure

instance MongoValue Bson.Function where
  toValue = Bson.Fun
  fromValue (Bson.Fun func) = pure func
  fromValue v               = expected "function" v

instance MongoValue () where
  toValue   ()        = Bson.Null
  fromValue Bson.Null = pure ()
  fromValue v         = expected "null" v

instance MongoValue Bool where
  toValue                 = Bson.Bool
  fromValue (Bson.Bool x) = pure x
  fromValue v             = expected "bool" v

instance MongoValue T.Text where
  toValue                   = Bson.String
  fromValue (Bson.String x) = pure x
  fromValue v               = expected "string" v

instance MongoValue LT.Text where
  toValue                   = Bson.String ∘ LT.toStrict
  fromValue (Bson.String x) = pure (LT.fromStrict x)
  fromValue v               = expected "string" v

instance (MongoValue α) ⇒ MongoValue (Maybe α) where
  toValue   Nothing   = Bson.Null
  toValue   (Just x)  = toValue x
  fromValue Bson.Null = pure Nothing
  fromValue x         = Just <$> fromValue x

instance (MongoValue α, MongoValue β) ⇒ MongoValue (Either α β) where
  toValue (Left  x) = Bson.Doc [ "_type" Bson.:= (Bson.String "left" ), "value" Bson.:= toValue x ]
  toValue (Right y) = Bson.Doc [ "_type" Bson.:= (Bson.String "right"), "value" Bson.:= toValue y ]

  fromValue (Bson.Doc doc) = do
    side ← doc `lookupThrow` "_type"
    case side of
      "left"  → Left  <$> doc `lookupThrow` "value"
      "right" → Right <$> doc `lookupThrow` "value"
      _       → throwError $ printf "expected either 'left' or 'right', found '%s'" (T.unpack side)
  fromValue v = expected "document" v

instance (MongoValue α) ⇒ MongoValue [α] where
  toValue = Bson.Array ∘ map toValue
  fromValue (Bson.Array xs) = mapM fromValue xs
  fromValue v               = expected "array" v

instance MongoValue UTCTime where
  toValue = Bson.UTC
  fromValue (Bson.UTC x) = pure x
  fromValue v            = expected "UTC" v

instance MongoValue BS.ByteString where
  toValue = Bson.Bin ∘ Bson.Binary
  fromValue (Bson.Bin (Bson.Binary x)) = pure x
  fromValue v                          = expected "binary" v

instance MongoValue LBS.ByteString where
  toValue = Bson.Bin ∘ Bson.Binary ∘ BS.concat ∘ LBS.toChunks
  fromValue (Bson.Bin (Bson.Binary x)) = pure $! LBS.fromChunks [x]
  fromValue v                          = expected "binary" v

instance MongoValue Word8 where
  toValue = Bson.Int32 ∘ fromIntegral
  fromValue (Bson.Int32 x) = pure $! fromIntegral x
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int32/Int64" v

instance MongoValue Word16 where
  toValue = Bson.Int32 ∘ fromIntegral
  fromValue (Bson.Int32 x) = pure $! fromIntegral x
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int32/Int64" v

instance MongoValue Word32 where
  toValue = Bson.Int64 ∘ fromIntegral
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int64" v

instance MongoValue Int where
  toValue = Bson.Int64 ∘ fromIntegral
  fromValue (Bson.Int32 x) = pure $! fromIntegral x
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int32/Int64" v

instance MongoValue Int8 where
  toValue = Bson.Int32 ∘ fromIntegral
  fromValue (Bson.Int32 x) = pure $! fromIntegral x
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int32/Int64" v

instance MongoValue Int16 where
  toValue = Bson.Int32 ∘ fromIntegral
  fromValue (Bson.Int32 x) = pure $! fromIntegral x
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int32/Int64" v

instance MongoValue Int32 where
  toValue = Bson.Int32 ∘ fromIntegral
  fromValue (Bson.Int32 x) = pure $! fromIntegral x
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int32/Int64" v

instance MongoValue Int64 where
  toValue = Bson.Int64 ∘ fromIntegral
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int64" v

instance MongoValue Integer where
  toValue = Bson.Int64 ∘ fromIntegral
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue (Bson.Float x) = pure $! round x
  fromValue v              = expected "Int64" v

instance (Integral α, MongoValue α) ⇒ MongoValue (Ratio α) where
  toValue     = toValue ∘ (numerator &&& denominator)
  fromValue v = uncurry (%) <$> fromValue v

instance MongoValue Float where
  toValue = Bson.Float ∘ realToFrac
  fromValue (Bson.Float x) = pure $! realToFrac x
  fromValue (Bson.Int32 x) = pure $! fromIntegral x
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue v              = expected "Double" v

instance MongoValue Double where
  toValue = Bson.Float
  fromValue (Bson.Float x) = pure x
  fromValue (Bson.Int32 x) = pure $! fromIntegral x
  fromValue (Bson.Int64 x) = pure $! fromIntegral x
  fromValue v              = expected "Double" v

instance MongoValue ObjectId where
  toValue = Bson.ObjId
  fromValue (Bson.ObjId x) = pure x
  fromValue v              = expected "ObjectId" v

instance (MongoValue α, MongoValue β) ⇒ MongoValue (α, β) where
  toValue (x, y) = Bson.Array [toValue x, toValue y]
  fromValue v @ (Bson.Array xs) =
    case xs of
      [x, y] → (,) <$> fromValue x <*> fromValue y
      _      → expected "array with two elements" v
  fromValue v = expected "array with two elements" v

-- | Instance of the 'MongoValue' type class for a map of strict 'T.Text' to
-- some value which is also an instance of the 'MongoValue' type class. This
-- type class instance is provided for more efficient storage of maps with
-- textual keys.
--
-- For example, the map @M.fromList [("cat", 1), ("dog", 2), ("mat", 3)]@ would
-- yield the JSON equivalent of:
--
-- @{ cat: 1, dog: 2, mat: 3 }@
instance (MongoValue α) ⇒ MongoValue (M.Map T.Text α) where
  toValue = Bson.Doc ∘ map (\(k, v) → k Bson.:= toValue v) ∘ M.toList
  fromValue (Bson.Doc doc) =
    M.fromList <$> mapM (\(k Bson.:= v) → (k, ) <$> fromValue v) doc
  fromValue v = expected "Document" v

instance (Ord α, MongoValue α, MongoValue β) ⇒ MongoValue (M.Map α β) where
  toValue = toValue ∘ M.toList
  fromValue v = M.fromList <$> fromValue v

-----------------------------------------------------------------------------------------------------------------------

#ifdef WITH_MT_SHARED

instance (MongoValue α) ⇒ MongoValue (NEList α) where
  toValue = toValue ∘ toListNE
  fromValue v = do
    xss ← fromValue v
    case xss of
      []       → throwError "expected non-empty list"
      (x : xs) → return (NEList x xs)

instance (MongoValue α) ⇒ MongoValue (Decimal α) where
  toValue (Decimal p v) = toValue (p, v)
  fromValue v = do
    (p, n) ← fromValue v
    pure $! Decimal p n

instance MongoValue Currency where
  toValue = toValue ∘ fromEnum
  fromValue v = toEnum <$> fromValue v

instance MongoValue Money where
  toValue (Money c v) = toValue (c, v)
  fromValue v = do
    (c, v') ← fromValue v
    pure $! Money c v'

instance MongoValue Password where
  toValue (Password (salt, hash)) =
    toValue (salt, TE.decodeUtf8 ∘ Base64U.encode $ hash)
  fromValue v = do
    (salt, encoded) ← fromValue v
    case Base64U.decode (TE.encodeUtf8 encoded) of
      Left msg → $(failFmt "failed to parse password: %s") msg
      Right ha → pure (Password (salt, ha))

#endif

-----------------------------------------------------------------------------------------------------------------------

instance Aeson.ToJSON Bson.ObjectId where
  toJSON = Aeson.String ∘ T.pack ∘ show

instance Aeson.FromJSON Bson.ObjectId where
  parseJSON (Aeson.String x) =
    case readMaybe (T.unpack x) of
      Just oid → pure oid
      Nothing  → fail $ printf "cannot read '%s' as an ObjectId" (T.unpack x)
  parseJSON _ = mzero

-----------------------------------------------------------------------------------------------------------------------

-- | Represents a null 'ObjectId' (all zeros).
nullObjectId ∷ ObjectId
nullObjectId = Oid 0 0

-- | Create a new object ID
newObjectId ∷ (MonadIO μ) ⇒ μ ObjectId
newObjectId = liftIO $ Bson.genObjectId

-----------------------------------------------------------------------------------------------------------------------

-- | When converting a MongoDB 'Bson.Value' to a concrete value it can be
-- useful to indicate that the wrong type of 'Bson.Value' was encountered,
-- including a description of what was found vs what was expected. This
-- function can be used to simply the construction of these messages.
expected ∷ (Monad μ) ⇒ String → Bson.Value → ErrorT String μ α
expected what was =
  throwError (printf "expected %s; found %s" what (describeType was))
  where
    describeType ∷ Bson.Value → String
    describeType (Bson.Float   _) = "float"
    describeType (Bson.String  _) = "string"
    describeType (Bson.Doc     _) = "Document"
    describeType (Bson.Array   n) = printf "Array (with %i elements)" (length n)
    describeType (Bson.Bin     _) = "Binary"
    describeType (Bson.Fun     _) = "Function"
    describeType (Bson.Uuid    _) = "UUID"
    describeType (Bson.Md5     _) = "MD5"
    describeType (Bson.UserDef _) = "UserDefined"
    describeType (Bson.ObjId   _) = "ObjectId"
    describeType (Bson.Bool    _) = "Bool"
    describeType (Bson.UTC     _) = "UTC"
    describeType (Bson.Null     ) = "null"
    describeType (Bson.RegEx   _) = "RegEx"
    describeType (Bson.JavaScr _) = "JavaScript"
    describeType (Bson.Sym     _) = "Symbol"
    describeType (Bson.Int32   _) = "Int32"
    describeType (Bson.Int64   _) = "Int64"
    describeType (Bson.Stamp   _) = "Stamp"
    describeType (Bson.MinMax  _) = "MinMax"

-----------------------------------------------------------------------------------------------------------------------

lookupMaybe ∷ Bson.Document → T.Text → Maybe Bson.Value
lookupMaybe doc name =
  maybe Nothing (Just ∘ Bson.value) $ find ((name ≡) ∘ Bson.label) doc

lookupThrow ∷ (Applicative μ, Monad μ, MongoValue α) ⇒ Bson.Document → T.Text → ErrorT String μ α
lookupThrow doc name =
  case lookupMaybe doc name of
    Just val → fromValue val
    Nothing  → throwError $ printf "could not find field '%s'" (T.unpack name)
