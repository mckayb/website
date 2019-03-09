{-# LANGUAGE FlexibleContexts #-}

module Handler.TagSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email

spec :: Spec
spec = withApp $ do
  describe "TagR" $ do
    it "Shouldn't let you visit the page if you aren't an admin" $ do
      get TagR
      statusIs 403

    it "Should let you visit the page if you are an admin" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" TagR

      htmlCount ".alert.alert-danger" 0
      htmlCount "input[placeholder=Tag Name]" 1
      htmlCount "input.btn.btn-primary[type=submit][value=Submit]" 1

    it "Should fail if there's no csrf token" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" TagR

      request $ do
        setMethod "POST"
        byLabelExact "Tag" "Commoner"
        setUrl TagR

      statusIs 403

    it "Should fail if the params aren't set properly" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" TagR

      request $ do
        addToken
        setMethod "POST"
        setUrl TagR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Should let you create a new tag" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" TagR

      tagsBefore <- runDB $ selectList ([] :: [Filter Tag]) []
      assertEq "no tags before" 0 $ length tagsBefore

      request $ do
        addToken
        byLabelExact "Tag" "Commoner"
        setMethod "POST"
        setUrl TagR

      tagsAfter <- runDB $ selectList ([] :: [Filter Tag]) []
      assertEq "single tag after" 1 $ length tagsAfter

    it "Shouldn't let you create a duplicate tag" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      _ <- createTag "Math"

      authGet em "mypassword" TagR

      tagsBefore <- runDB $ selectList ([] :: [Filter Tag]) []
      assertEq "only 1 tag before" 1 $ length tagsBefore

      request $ do
        addToken
        byLabelExact "Tag" "Math"
        setMethod "POST"
        setUrl TagR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "A tag already exists with that name"

      tagsAfter <- runDB $ selectList ([] :: [Filter Tag]) []
      assertEq "only 1 tag after" 1 $ length tagsAfter