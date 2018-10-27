module Helpers.Auth where

import Prelude
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
  mRoleJson <- lookupSession roleSessionKey
  let mUser = decode =<< cs <$> mUserJson :: Maybe (Entity User)
  let mRole = decode =<< cs <$> mRoleJson :: Maybe Role
  return $ case validateAdmin <$> mUser <*> mRole of
    Just u -> u
    Nothing -> Unauthorized "You are not allowed to access this page"
  where
    validateAdmin _ role =
      if roleName role == "Admin"
        then Authorized
        else Unauthorized "You are not allowed to access this page"
