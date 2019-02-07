module Helpers.Session where

import Import
import qualified Helpers.Auth as Auth
import qualified Data.String.Conversions as Conversions
import qualified Data.Aeson as Aeson

requireUser :: Handler (Entity User)
requireUser = do
  mUser <- Auth.getCurrentUser
  case mUser of
    Just u -> return u
    Nothing -> permissionDenied "You must login to access this page"

requireAdminUser :: Handler (Entity User)
requireAdminUser = do
  user <- requireUser
  mRole <- Auth.getCurrentRole
  case mRole of
    Nothing -> permissionDenied "You must be an admin to access this page"
    Just role ->
      if (roleName . entityVal) role == "Admin" && ((userRoleId . entityVal) user == entityKey role)
        then return user
        else permissionDenied "You must be an admin to access this page"

optionalAdminUser :: Handler (Maybe (Entity User))
optionalAdminUser = do
  mUser <- Auth.getCurrentUser
  mRole <- Auth.getCurrentRole
  case mUser of
    Nothing -> return Nothing
    Just user -> 
      case mRole of
        Nothing -> return Nothing
        Just role ->
          if (roleName . entityVal) role == "Admin" && ((userRoleId . entityVal) user == entityKey role)
            then return mUser
            else return Nothing

isAdmin :: Handler Bool
isAdmin = do
  mUser <- Auth.getCurrentUser
  mRole <- Auth.getCurrentRole
  case mUser of
    Nothing -> return False
    Just user -> 
      case mRole of
        Nothing -> return False
        Just role -> return $ (roleName . entityVal) role == "Admin" && ((userRoleId . entityVal) user == entityKey role)

keepLoggedIn :: Entity User -> Entity Role -> Handler ()
keepLoggedIn user role = do
  setSession userSessionKey $ (Conversions.cs . Aeson.encode) user
  setSession roleSessionKey $ (Conversions.cs . Aeson.encode) role
  return ()