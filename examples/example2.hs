{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies                           #-}
module Main where

import Control.Monad
import Data.Default
import qualified Data.Text.IO as T
import Massive.Database.MongoDB
import System.IO
import Text.Printf


import ExampleTypes

main ∷ IO ()
main = do
  -- Using the same database as in example1, we create a pool of connections
  -- that we can use in the following code.
  withMongoDBPool (def { dbConfigDatabase = "mt-mongodb-example1" }) $ \pool → do

    -- First off, ask the user for a users alias.
    alias ← do { putStr "Enter User Alias: "; hFlush stdout; T.getLine }

    -- Select a collection of users from the database where the user alias is the
    -- same as the one just provided. We use filters in this instance to match on
    -- the user name.
    Collection found ← runMongoDBConn pool $ select (filters [UserAlias `eq` alias]) []

    -- Alternatively, we could use MongoDB's query syntax:
    --Collection found ← runMongoDBConn pool $ select [mongo|{userAlias: #{alias}}|] []

    -- Pretty-print what we found.
    void $ printf "\nFound %i matches:\n\n" (length found)
    mapM_ prettyUser found
