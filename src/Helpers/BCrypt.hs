module Helpers.BCrypt where

import Prelude
import Database.Persist.Sql (PersistField, PersistFieldSql)
import Data.Text (Text)
import Crypto.BCrypt (HashingPolicy(HashingPolicy))
import qualified Safe (fromJustNote)
import qualified Crypto.BCrypt as CB
import qualified Data.Text.Encoding as TE

newtype BCrypt =
  BCrypt { unBCrypt ::  Text }
  deriving (Eq, PersistField, PersistFieldSql, Show)

policy :: HashingPolicy
policy = HashingPolicy
  { CB.preferredHashCost = 12
  , CB.preferredHashAlgorithm = "$2a$"
  }

hashPassword :: Text -> IO BCrypt
hashPassword rawPassword = do
  mPassword <- CB.hashPasswordUsingPolicy policy $ TE.encodeUtf8 rawPassword
  return $ BCrypt $ TE.decodeUtf8 $ Safe.fromJustNote "Invalid hashing policy" mPassword

passwordMatches :: BCrypt -> Text -> Bool
passwordMatches hash' pass = 
  CB.validatePassword (TE.encodeUtf8 $ unBCrypt hash') (TE.encodeUtf8 pass)