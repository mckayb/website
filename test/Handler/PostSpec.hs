{-# LANGUAGE FlexibleContexts #-}

module Handler.PostSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email
import qualified Helpers.Slug as Slug
import qualified Database.Persist.Sql as Sql (fromSqlKey)

spec :: Spec
spec = withApp $ do
  describe "PostR" $ do
    it "Should not let you go to the page without a user that is currently logged in" $ do
      get PostR
      statusIs 403

    it "Should let you go to the page if you are logged in" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost em "mypassword"

      bodyContains "Create Post"

    it "Shouldn't let you preview a post if you don't have all the required fields set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
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

    it "Shouldn't let you publish a post if you don't have the required fields set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
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
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "Foo"
      let Right slug = Slug.mkSlug "the-slug"

      goToPost em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      assertEq "No posts before we preview" 0 $ length postsBefore

      request $ do
        addToken
        addPostParam "action" "Preview"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "## This is the content"
        byLabelExact "Slug" (Slug.unSlug slug)
        byLabelExact "Tags" (pack $ show $ Sql.fromSqlKey $ entityKey tag)
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
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "foo"
      let Right slug = Slug.mkSlug "the-slug"

      goToPost em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsBefore <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "No posts before we publish" 0 $ length postsBefore
      assertEq "No post tags before we publish" 0 $ length postTagsBefore

      request $ do
        addToken
        addPostParam "action" "Publish"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "## This is the content"
        byLabelExact "Slug" (Slug.unSlug slug)
        byLabelExact "Tags" (pack $ show $ Sql.fromSqlKey $ entityKey tag)
        setMethod "POST"
        setUrl PostR

      statusIs 200

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsAfter <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "added a new post" 1 $ length postsAfter
      assertEq "added a new post tag" 1 $ length postTagsAfter
      let Just post' = listToMaybe postsAfter
      let Just postTag' = listToMaybe postTagsAfter

      assertEq "Post title" ((postTitle . entityVal) post') "This is the title"
      assertEq "Post content" ((postContent . entityVal) post') "## This is the content"
      assertEq "Post user" ((postUserId . entityVal) post') (entityKey user)
      assertEq "Post slug" ((postSlug . entityVal) post') slug

      assertEq "PostTag post id" ((postTagPostId . entityVal) postTag') (entityKey post')
      assertEq "PostTag tag id" ((postTagTagId . entityVal) postTag') (entityKey tag)
      bodyContains "Successfully published new post"
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
