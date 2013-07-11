{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies                           #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Default
import qualified Data.Text.IO as T
import Data.Time
import Massive.Database.MongoDB
import System.IO
import Text.Printf

import ExampleTypes

getUser ∷ IO User
getUser = do
  putStr "Enter Name : "; hFlush stdout; name  <- T.getLine
  putStr "Enter Alias: "; hFlush stdout; alias <- T.getLine
  User name alias <$> getCurrentTime

main ∷ IO ()
main = do
  withMongoDBPool (def { dbConfigDatabase = "mt-mongodb-example1" }) $ \pool → do
    Collection users ← runMongoDBConn pool $ findAll
    void $ printf "\nDB Contains %i users:\n\n" (length users)
    mapM_ prettyUser users

    user ← getUser
    key  ← runMongoDBConn pool $ insert user
    printf "New key: %s\n" (show key)
