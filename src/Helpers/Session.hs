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
    Just r ->
      case roleName r of
        "Admin" -> return user
        _ -> permissionDenied "You must be an admin to access this page"

keepLoggedIn :: Entity User -> Role -> Handler ()
keepLoggedIn user role = do
  setSession userSessionKey $ (Conversions.cs . Aeson.encode) user
  setSession roleSessionKey $ (Conversions.cs . Aeson.encode) role
  return ()