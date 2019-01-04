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
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToTag em "mypassword"

      htmlCount ".alert.alert-danger" 0
      htmlCount "input[placeholder=Tag Name]" 1
      htmlCount "input.btn.btn-primary[type=submit][value=Submit]" 1

    it "Should fail if there's no csrf token" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToTag em "mypassword"

      request $ do
        setMethod "POST"
        byLabelExact "Tag" "Commoner"
        setUrl TagR

      statusIs 403

    it "Should fail if the params aren't set properly" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToTag em "mypassword"

      request $ do
        addToken
        setMethod "POST"
        setUrl TagR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "There was an error submitting your form."

    it "Should let you create a new tag" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToTag em "mypassword"

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
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      _ <- createTag "Math"

      goToTag em "mypassword"

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

  where
    goToTag em pass = do
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
      get TagR
      statusIs 200