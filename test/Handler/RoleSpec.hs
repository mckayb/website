{-# LANGUAGE FlexibleContexts #-}

module Handler.RoleSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email

spec :: Spec
spec = withApp $ do
  describe "RoleR" $ do
    it "Shouldn't let you visit the page if you aren't an admin" $ do
      get RoleR
      statusIs 403

    it "Should let you visit the page if you are an admin" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToRole em "mypassword"

      htmlCount ".alert.alert-danger" 0
      htmlCount "input[placeholder=Role Name]" 1
      htmlCount "input.btn.btn-primary[type=submit][value=Submit]" 1

    it "Should fail if there's no csrf token" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToRole em "mypassword"

      request $ do
        setMethod "POST"
        byLabelExact "Role" "Commoner"
        setUrl RoleR

      statusIs 403

    it "Should fail if the params aren't set properly" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToRole em "mypassword"

      request $ do
        addToken
        setMethod "POST"
        setUrl RoleR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "There was an error submitting your form."

    it "Should let you create a new role" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToRole em "mypassword"

      rolesBefore <- runDB $ selectList ([] :: [Filter Role]) []
      assertEq "only 1 role before" 1 $ length rolesBefore

      request $ do
        addToken
        byLabelExact "Role" "Commoner"
        setMethod "POST"
        setUrl RoleR

      rolesAfter <- runDB $ selectList ([] :: [Filter Role]) []
      assertEq "added a new role" 2 $ length rolesAfter

    it "Shouldn't let you create a duplicate role" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToRole em "mypassword"

      rolesBefore <- runDB $ selectList ([] :: [Filter Role]) []
      assertEq "only 1 role before" 1 $ length rolesBefore

      request $ do
        addToken
        byLabelExact "Role" "Admin"
        setMethod "POST"
        setUrl RoleR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "A role already exists with that name"

      rolesAfter <- runDB $ selectList ([] :: [Filter Role]) []
      assertEq "only 1 role after" 1 $ length rolesAfter

  where
    goToRole em pass = do
      get LoginR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" $ Email.unEmail em
        byLabelExact "Password" pass
        setMethod "POST"
        setUrl LoginR

      statusIs 303
      _ <- followRedirect
      statusIs 200
      get RoleR
      statusIs 200