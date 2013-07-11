{-# LANGUAGE UnicodeSyntax, OverloadedStrings, TemplateHaskell, QuasiQuotes, TypeFamilies                           #-}
module ExampleTypes where

import Control.Monad
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Massive.Database.MongoDB
import System.Locale
import Text.Printf

data User = User { userName      ∷ Text
                 , userAlias     ∷ Text
                 , userCreatedAt ∷ UTCTime
                 }

asMongoEntity ''User useDefaults

prettyUser ∷ Entity User → IO ()
prettyUser (Entity userId user) = do
  void $ printf "%s:\n" (show userId)
  void $ printf "  User Name : %s\n" (T.unpack (userName user))
  void $ printf "  User Alias: %s\n" (T.unpack (userAlias user))
  void $ printf "  Created At: %s\n\n" (formatTime defaultTimeLocale "%F %T" (userCreatedAt user))
  return ()
