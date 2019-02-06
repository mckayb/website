module Helpers.Auth where

import Prelude
import Yesod.Core.Handler (HandlerFor)
import Yesod.Core.Types (AuthResult(Authorized, Unauthorized))
import Model (User, Role)
import Database.Persist.Types (Entity, entityVal, entityKey)
import qualified Yesod.Core.Handler as Handler
import qualified Settings
import qualified Model
import qualified Data.String.Conversions as Conversions
import qualified Data.Aeson as Aeson
import qualified Data.Maybe as Maybe

isAuthenticatedBasic :: HandlerFor site AuthResult
isAuthenticatedBasic = do
  mUser <- getCurrentUser
  return $ case mUser of
    Just _ -> Authorized
    Nothing -> Unauthorized "You must login to access this page"

isAuthenticatedAdmin :: HandlerFor site AuthResult
isAuthenticatedAdmin = do
  mUser <- getCurrentUser
  mRole <- getCurrentRole
  return $ Maybe.fromMaybe (Unauthorized "You are not allowed to access this page") (validateAdmin <$> mUser <*> mRole)
  where
    validateAdmin user role =
      if (Model.roleName . entityVal) role == "Admin" && (entityKey role) == (Model.userRoleId . entityVal) user
        then Authorized
        else Unauthorized "You are not allowed to access this page"

getCurrentUser :: HandlerFor site (Maybe (Entity User))
getCurrentUser = do
  mUserJson <- Handler.lookupSession Settings.userSessionKey
  return $ Aeson.decode =<< Conversions.cs <$> mUserJson

getCurrentRole :: HandlerFor site (Maybe (Entity Role))
getCurrentRole = do
  mRoleJson <- Handler.lookupSession Settings.roleSessionKey
  return $ Aeson.decode =<< Conversions.cs <$> mRoleJson
