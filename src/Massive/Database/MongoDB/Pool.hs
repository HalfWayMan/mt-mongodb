{-# LANGUAGE UnicodeSyntax, OverloadedStrings, DeriveDataTypeable                                                   #-}
-----------------------------------------------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB.Pool
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Database connection pools.
--
-----------------------------------------------------------------------------------------------------------------------

module Massive.Database.MongoDB.Pool ( ConnectionPool
                                     , createMongoDBPool
                                     , withMongoDBPool
                                     , runMongoDBConn
                                     , MongoDBError (..)
                                     ) where

import           Prelude.Unicode
import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import qualified Data.Text                       as T
import           Data.Typeable
import qualified Database.MongoDB                as MongoDB
import qualified System.IO.Pool                  as Pool

import           Massive.Database.MongoDB.Config

-----------------------------------------------------------------------------------------------------------------------

type ConnectionPool = (Pool.Pool IOError MongoDB.Pipe, MongoDB.Database)

-----------------------------------------------------------------------------------------------------------------------

connectMongoDB ∷ DatabaseConfig → MongoDB.IOE MongoDB.Pipe
connectMongoDB config = do
  conn ← MongoDB.connect (MongoDB.Host (T.unpack (dbConfigHost config)) (MongoDB.PortNumber $ fromIntegral $ dbConfigPort config))
  _    ← case dbConfigUserPass config of
           Just (user, pass) → MongoDB.access conn MongoDB.UnconfirmedWrites
                                              (dbConfigDatabase config)
                                              (MongoDB.auth user pass)
           Nothing           → return undefined
  return conn

createMongoDBPool ∷ (MonadIO μ, Applicative μ) ⇒ DatabaseConfig → μ ConnectionPool
createMongoDBPool config = do
  pool ← liftIO $ Pool.newPool Pool.Factory { Pool.newResource  = connectMongoDB config
                                            , Pool.killResource = MongoDB.close
                                            , Pool.isExpired    = MongoDB.isClosed
                                            }
                               (dbConfigPoolSize config)
  return (pool, dbConfigDatabase config)

withMongoDBPool ∷ (MonadIO μ, Applicative μ) ⇒ DatabaseConfig → (ConnectionPool → μ α) → μ α
withMongoDBPool config connectionReader = do
  pool ← createMongoDBPool config
  connectionReader pool

runMongoDBConn ∷ (MonadIO μ) ⇒ ConnectionPool → MongoDB.Action μ α → μ α
runMongoDBConn (pool, databaseName) action = do
  pipe ← liftIO $ MongoDB.runIOE $ Pool.aResource pool
  res  ← MongoDB.access pipe (MongoDB.ConfirmWrites ["j" MongoDB.=: True]) databaseName action
  either (liftIO ∘ throwIO ∘ MongoDBError ∘ T.pack ∘ show) return res

-----------------------------------------------------------------------------------------------------------------------

data MongoDBError = MongoDBError T.Text
                  deriving (Show, Typeable)

instance Exception MongoDBError

