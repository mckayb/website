module Helpers.Auth where

import Yesod.Core.Handler
import Yesod.Core.Types
import Settings
import Model
import Database.Persist.Types
import Data.String.Conversions
import Data.Aeson

isAuthenticatedBasic :: HandlerFor site AuthResult
isAuthenticatedBasic = do
  mUser <- lookupSession userSessionKey
  return $ case mUser of
    Just _ -> Authorized
    Nothing -> Unauthorized "You must login to access this page"

isAuthenticatedAdmin :: HandlerFor site AuthResult
isAuthenticatedAdmin = do
  mUserJson <- lookupSession userSessionKey
  let mUser = decode =<< cs <$> mUserJson :: Maybe (Entity User)
  return $ case mUser of
    Just user -> do
      Authorized
    Nothing -> Unauthorized "You are not allowed to access this page"
