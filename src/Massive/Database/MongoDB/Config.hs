{-# LANGUAGE UnicodeSyntax, OverloadedStrings                               #-}
-------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.Config
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- MongoDB configuration.
--
-------------------------------------------------------------------------------

module Massive.Database.MongoDB.Config ( DatabaseConfig (..)
                                       ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                           as Aeson
import qualified Data.Bson                            as Bson
import           Data.Default
import qualified Data.Text                            as T

import           Massive.Database.MongoDB.MongoEntity

-------------------------------------------------------------------------------

data DatabaseConfig = DatabaseConfig { dbConfigHost     ∷ T.Text
                                     , dbConfigDatabase ∷ T.Text
                                     , dbConfigUserPass ∷ Maybe (T.Text, T.Text)
                                     , dbConfigPoolSize ∷ Int
                                     }

instance Default DatabaseConfig where
  def = DatabaseConfig { dbConfigHost     = "localhost"
                       , dbConfigDatabase = "test"
                       , dbConfigUserPass = Nothing
                       , dbConfigPoolSize = 5
                       }

instance MongoValue DatabaseConfig where
  toValue dbConf = Bson.Doc [ "host"     ≕ dbConfigHost     dbConf
                            , "database" ≕ dbConfigDatabase dbConf
                            , "userpass" ≕ dbConfigUserPass dbConf
                            , "poolSize" ≕ dbConfigPoolSize dbConf
                            ]

  fromValue (Bson.Doc doc) =
    DatabaseConfig <$> doc .:  "host"
                   <*> doc .:  "database"
                   <*> doc .:? "userpass"
                   <*> doc .:  "poolSize"
  fromValue v = expected "Object" v

instance Aeson.ToJSON DatabaseConfig where
  toJSON dbConf = Aeson.object [ "host"     Aeson..= dbConfigHost     dbConf
                               , "database" Aeson..= dbConfigDatabase dbConf
                               , "userpass" Aeson..= dbConfigUserPass dbConf
                               , "poolSize" Aeson..= dbConfigPoolSize dbConf
                               ]

instance Aeson.FromJSON DatabaseConfig where
  parseJSON (Aeson.Object obj) =
    DatabaseConfig <$> obj Aeson..:  "host"
                   <*> obj Aeson..:  "database"
                   <*> obj Aeson..:? "userpass"
                   <*> obj Aeson..:  "poolSize"
  parseJSON _ = mzero
