module Helpers.Session where

import Import
import Helpers.Auth
import Data.Aeson (encode)
import Data.String.Conversions (cs)

requireUser :: Handler (Entity User)
requireUser = do
  mUser <- getCurrentUser
  case mUser of
    Just u -> return u
    Nothing -> permissionDenied "You must login to access this page"

requireAdminUser :: Handler (Entity User)
requireAdminUser = do
  user <- requireUser
  mRole <- getCurrentRole
  case mRole of
    Nothing -> permissionDenied "You must be an admin to access this page"
    Just r ->
      case roleName r of
        "Admin" -> return user
        _ -> permissionDenied "You must be an admin to access this page"

keepLoggedIn :: Entity User -> Role -> Handler ()
keepLoggedIn user role = do
  setSession userSessionKey $ (cs . encode) user
  setSession roleSessionKey $ (cs . encode) role
  return ()