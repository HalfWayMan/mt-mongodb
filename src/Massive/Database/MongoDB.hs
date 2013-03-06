-- -*- mode: Haskell; fill-column: 79; default-justification: left;         -*-
{-# LANGUAGE UnicodeSyntax                                                  #-}
-------------------------------------------------------------------------------
-- |
-- Module     : Massive.Database.MongoDB
-- Copyright  : (C) 2012 Massive Tactical Limited
-- License    : BSD3
-- Maintainer : Blake Rain <blake.rain@massivetactical.com>
--
-- Yet another  interface for MongoDB.  This one  is simpler than  any previous
-- version.
--
-------------------------------------------------------------------------------

module Massive.Database.MongoDB ( module Massive.Database.MongoDB.Config
                                , module Massive.Database.MongoDB.Expr
                                , module Massive.Database.MongoDB.MongoEntity
                                , module Massive.Database.MongoDB.Operations
                                , module Massive.Database.MongoDB.Pool
                                , module Massive.Database.MongoDB.Types
                                ) where

import Massive.Database.MongoDB.Config
import Massive.Database.MongoDB.Expr
import Massive.Database.MongoDB.MongoEntity
import Massive.Database.MongoDB.Operations
import Massive.Database.MongoDB.Pool
import Massive.Database.MongoDB.Types

