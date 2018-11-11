module Helpers.Auth where

import Prelude
import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Types (AuthResult(Authorized, Unauthorized))
import Model (User, Role)
import Database.Persist.Types (Entity)
import qualified Yesod.Core.Handler as Handler
import qualified Settings
import qualified Model
import qualified Data.String.Conversions as Conversions
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe

isAuthenticatedBasic :: HandlerFor site AuthResult
isAuthenticatedBasic = do
  mUser <- Handler.lookupSession Settings.userSessionKey
  return $ case mUser of
    Just _ -> Authorized
    Nothing -> Unauthorized "You must login to access this page"

isAuthenticatedAdmin :: HandlerFor site AuthResult
isAuthenticatedAdmin = do
  mUserJson <- Handler.lookupSession Settings.userSessionKey
  mRoleJson <- Handler.lookupSession Settings.roleSessionKey
  let mUser = Aeson.decode =<< Conversions.cs <$> mUserJson :: Maybe (Entity User)
  let mRole = Aeson.decode =<< Conversions.cs <$> mRoleJson :: Maybe Role
  return $ Maybe.fromMaybe (Unauthorized "You are not allowed to access this page") (validateAdmin <$> mUser <*> mRole)
  where
    validateAdmin _ role =
      if Model.roleName role == "Admin"
        then Authorized
        else Unauthorized "You are not allowed to access this page"

getCurrentUser :: HandlerFor site (Maybe (Entity User))
getCurrentUser = do
  mUserJson <- Handler.lookupSession Settings.userSessionKey
  return $ Aeson.decode =<< Conversions.cs <$> mUserJson

getCurrentRole :: HandlerFor site (Maybe Role)
getCurrentRole = do
  mRoleJson <- Handler.lookupSession Settings.roleSessionKey
  return $ Aeson.decode =<< Conversions.cs <$> mRoleJson
