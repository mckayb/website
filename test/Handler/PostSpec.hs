{-# LANGUAGE FlexibleContexts #-}

module Handler.PostSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email

spec :: Spec
spec = withApp $ do
  describe "PostR" $ do
    it "Should not let you go to the page without a user that is currently logged in" $ do
      get PostR
      statusIs 403

    it "Should let you go to the page if you are logged in" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ (Email.mkEmail . pack) <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost em "mypassword"

      bodyContains "Create Post"

    it "Shouldn't let you preview a post if you don't have the title or content set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ (Email.mkEmail . pack) <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Preview"
        setMethod "POST"
        setUrl PostR

      statusIs 200
      bodyContains "Something went wrong"

    it "Shouldn't let you publish a post if you don't have the title or content set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ (Email.mkEmail . pack) <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Publish"
        setMethod "POST"
        setUrl PostR

      statusIs 200
      bodyContains "Something went wrong"

    it "Should let you preview the post you're about to publish" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ (Email.mkEmail . pack) <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      assertEq "No posts before we preview" 0 $ length postsBefore

      request $ do
        addToken
        addPostParam "action" "Preview"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "## This is the content"
        setMethod "POST"
        setUrl PostR

      bodyContains "Preview"
      htmlCount ".post__preview" 1
      htmlAnyContain "h1" "This is the title"
      htmlAnyContain "h2" "This is the content"

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      assertEq "No posts after we preview" 0 $ length postsAfter

    it "Should let you publish the post successfully" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ (Email.mkEmail . pack) <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      assertEq "No posts before we publish" 0 $ length postsBefore

      request $ do
        addToken
        addPostParam "action" "Publish"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "## This is the content"
        setMethod "POST"
        setUrl PostR

      statusIs 200

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      assertEq "added a new post" 1 $ length postsAfter
      let Just post' = listToMaybe postsAfter

      assertEq "Post title" ((postTitle . entityVal) post') "This is the title"
      assertEq "Post content" ((postContent . entityVal) post') "## This is the content"
      assertEq "Post user" ((postUserId . entityVal) post') (entityKey user)

  where
    goToPost em pass = do
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
      get PostR
      statusIs 200
