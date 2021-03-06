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
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" RoleR

      htmlCount ".alert.alert-danger" 0
      htmlCount "input[placeholder=Role Name]" 1
      htmlCount "input.btn.btn-primary[type=submit][value=Submit]" 1

    it "Should fail if there's no csrf token" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" RoleR

      request $ do
        setMethod "POST"
        byLabelExact "Role" "Commoner"
        setUrl RoleR

      statusIs 403

    it "Should fail if the params aren't set properly" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" RoleR

      request $ do
        addToken
        setMethod "POST"
        setUrl RoleR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Should let you create a new role" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" RoleR

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
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" RoleR

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