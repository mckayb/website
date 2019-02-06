{-# LANGUAGE FlexibleContexts #-}

module Handler.PostSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email
import qualified Helpers.Slug as Slug
import qualified Helpers.Markdown as Markdown
import qualified Database.Persist.Sql as Sql (fromSqlKey)

spec :: Spec
spec = withApp $ do
  describe "CreatePostR" $ do
    it "Should not let you go to the page without a user that is currently logged in" $ do
      get CreatePostR
      statusIs 403

    it "Should let you go to the page if you are logged in" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost CreatePostR em "mypassword"

      bodyContains "Create Post"

    it "Shouldn't let you preview a post if you don't have all the required fields set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost CreatePostR em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Preview"
        setMethod "POST"
        setUrl CreatePostR

      statusIs 200
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Shouldn't let you save a post as a draft if you don't have the required fields set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost CreatePostR em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Save as Draft"
        setMethod "POST"
        setUrl CreatePostR

      statusIs 200
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Shouldn't let you publish a post if you don't have the required fields set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      goToPost CreatePostR em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Publish"
        setMethod "POST"
        setUrl CreatePostR

      statusIs 200
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Should throw errors if you try to preview the post with invalid markdown" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "Foo"
      let Right slug = Slug.mkSlug "the-slug"

      goToPost CreatePostR em "mypassword"
      request $ do
        addToken
        addPostParam "action" "Preview"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "```This is the content"
        byLabelExact "Slug" (Slug.unSlug slug)
        byLabelExact "Tags" (pack $ show $ Sql.fromSqlKey $ entityKey tag)
        setMethod "POST"
        setUrl CreatePostR

      statusIs 200

    it "Should let you preview the post you're about to publish" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "Foo"
      let Right slug = Slug.mkSlug "the-slug"

      goToPost CreatePostR em "mypassword"

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
        setUrl CreatePostR

      statusIs 200
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

      goToPost CreatePostR em "mypassword"

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
        setUrl CreatePostR

      statusIs 200

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsAfter <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "added a new post" 1 $ length postsAfter
      assertEq "added a new post tag" 1 $ length postTagsAfter
      let Just post' = listToMaybe postsAfter
      let Just postTag' = listToMaybe postTagsAfter

      assertEq "Post title" ((postTitle . entityVal) post') "This is the title"
      assertEq "Post content" ((Markdown.unMarkdown . postContent . entityVal) post') "## This is the content"
      assertEq "Post user" ((postUserId . entityVal) post') (entityKey user)
      assertEq "Post slug" ((postSlug . entityVal) post') slug
      assertEq "Post published" ((postPublished . entityVal) post') True

      assertEq "PostTag post id" ((postTagPostId . entityVal) postTag') (entityKey post')
      assertEq "PostTag tag id" ((postTagTagId . entityVal) postTag') (entityKey tag)
      bodyContains "Successfully published new post"

    it "Should let you save the post as a draft successfully" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "foo"
      let Right slug = Slug.mkSlug "the-slug"

      goToPost CreatePostR em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsBefore <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "No posts before we publish" 0 $ length postsBefore
      assertEq "No post tags before we publish" 0 $ length postTagsBefore

      request $ do
        addToken
        addPostParam "action" "Save as Draft"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "## This is the content"
        byLabelExact "Slug" (Slug.unSlug slug)
        byLabelExact "Tags" (pack $ show $ Sql.fromSqlKey $ entityKey tag)
        setMethod "POST"
        setUrl CreatePostR

      statusIs 200

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsAfter <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "added a new post" 1 $ length postsAfter
      assertEq "added a new post tag" 1 $ length postTagsAfter
      let Just post' = listToMaybe postsAfter
      let Just postTag' = listToMaybe postTagsAfter

      assertEq "Post title" ((postTitle . entityVal) post') "This is the title"
      assertEq "Post content" ((Markdown.unMarkdown . postContent . entityVal) post') "## This is the content"
      assertEq "Post user" ((postUserId . entityVal) post') (entityKey user)
      assertEq "Post slug" ((postSlug . entityVal) post') slug
      assertEq "Post published" ((postPublished . entityVal) post') False

      assertEq "PostTag post id" ((postTagPostId . entityVal) postTag') (entityKey post')
      assertEq "PostTag tag id" ((postTagTagId . entityVal) postTag') (entityKey tag)
      bodyContains "Successfully saved draft"

  describe "EditPostR" $ do
    it "Should not let you go to the page without a user that is currently logged in" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      tag <- createTag "foo"
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True
      _ <- createPostTag post' tag

      get (EditPostR (entityKey post'))
      statusIs 403

    it "Should let you go to the page if you are logged in" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True

      goToPost (EditPostR (entityKey post')) em "mypassword"

      bodyContains "Create Post"

    it "Shouldn't let you preview the post if you don't have all the required fields set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      tag <- createTag "foo"
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True
      _ <- createPostTag post' tag

      goToPost (EditPostR (entityKey post')) em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Preview"
        setMethod "POST"
        setUrl $ EditPostR (entityKey post')

      statusIs 200
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Shouldn't let you save a post as a draft if you don't have the required fields set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      time <- liftIO getCurrentTime
      tag <- createTag "foo"
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True
      _ <- createPostTag post' tag

      goToPost (EditPostR (entityKey post')) em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Save as Draft"
        setMethod "POST"
        setUrl $ EditPostR (entityKey post')

      statusIs 200
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Shouldn't let you publish a post if you don't have the required fields set" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      tag <- createTag "foo"
      _ <- createPassword user "mypassword"
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True
      _ <- createPostTag post' tag

      goToPost (EditPostR (entityKey post')) em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Revise and Publish"
        setMethod "POST"
        setUrl $ EditPostR (entityKey post')

      statusIs 200
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Should throw errors if you try to preview the post with invalid markdown" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "Foo"
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-slug"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True
      _ <- createPostTag post' tag

      goToPost (EditPostR (entityKey post')) em "mypassword"

      request $ do
        addToken
        addPostParam "action" "Preview"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "```This is the content"
        byLabelExact "Slug" (Slug.unSlug slug)
        byLabelExact "Tags" (pack $ show $ Sql.fromSqlKey $ entityKey tag)
        setMethod "POST"
        setUrl $ EditPostR (entityKey post')

      statusIs 200

    it "Should let you preview the post you're about to publish" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "foo"
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-slug"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True
      _ <- createPostTag post' tag

      goToPost (EditPostR (entityKey post')) em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      assertEq "Single post before we preview" 1 $ length postsBefore

      request $ do
        addToken
        addPostParam "action" "Preview"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "## This is the content"
        byLabelExact "Slug" (Slug.unSlug slug)
        byLabelExact "Tags" (pack $ show $ Sql.fromSqlKey $ entityKey tag)
        setMethod "POST"
        setUrl $ EditPostR (entityKey post')

      statusIs 200
      bodyContains "Preview"
      htmlCount ".post__preview" 1
      htmlAnyContain "h1" "This is the title"
      htmlAnyContain "h2" "This is the content"

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      assertEq "Single post after we preview" 1 $ length postsAfter

    it "Should let you publish the post successfully" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "foo"
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-slug"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time False
      _ <- createPostTag post' tag

      goToPost (EditPostR (entityKey post')) em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsBefore <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "Single post before we publish" 1 $ length postsBefore
      assertEq "Single post tag before we publish" 1 $ length postTagsBefore

      request $ do
        addToken
        addPostParam "action" "Revise and Publish"
        byLabelExact "Title" "New Title"
        byLabelExact "Content" "New Content"
        byLabelExact "Slug" "new-slug"
        byLabelExact "Tags" (pack $ show $ Sql.fromSqlKey $ entityKey tag)
        setMethod "POST"
        setUrl $ EditPostR (entityKey post')

      statusIs 200

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsAfter <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "kept the same post" 1 $ length postsAfter
      assertEq "kept the same post tag" 1 $ length postTagsAfter
      let Just postAfter' = listToMaybe postsAfter
      let Just postTag' = listToMaybe postTagsAfter

      assertEq "Post ids equal before and after" (entityKey post') (entityKey postAfter')
      assertEq "Post title" ((postTitle . entityVal) post') "The Post"
      assertEq "PostAfter title" ((postTitle . entityVal) postAfter') "New Title"
      assertEq "Post content" ((Markdown.unMarkdown . postContent . entityVal) post') "First\nSecond\nThird"
      assertEq "PostAfter content" ((Markdown.unMarkdown . postContent . entityVal) postAfter') "New Content"
      assertEq "Post user" ((postUserId . entityVal) post') (entityKey user)
      assertEq "PostAfter user" ((postUserId . entityVal) postAfter') (entityKey user)
      assertEq "Post slug" ((Slug.unSlug . postSlug . entityVal) post') "the-slug"
      assertEq "PostAfter slug" ((Slug.unSlug . postSlug . entityVal) postAfter') "new-slug"
      assertEq "Post published" ((postPublished . entityVal) post') False
      assertEq "PostAfter published" ((postPublished . entityVal) postAfter') True

      assertEq "PostTag post id" ((postTagPostId . entityVal) postTag') (entityKey post')
      assertEq "PostTag tag id" ((postTagTagId . entityVal) postTag') (entityKey tag)
      bodyContains "Successfully published new post"

    it "Should let you save the post as a draft successfully" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "foo"
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-slug"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True
      _ <- createPostTag post' tag

      goToPost (EditPostR (entityKey post')) em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsBefore <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "Single post before we publish" 1 $ length postsBefore
      assertEq "Single post tag before we publish" 1 $ length postTagsBefore

      request $ do
        addToken
        addPostParam "action" "Save as Draft"
        byLabelExact "Title" "This is the title"
        byLabelExact "Content" "## This is the content"
        byLabelExact "Slug" "new-slug"
        byLabelExact "Tags" (pack $ show $ Sql.fromSqlKey $ entityKey tag)
        setMethod "POST"
        setUrl $ EditPostR (entityKey post')

      statusIs 200

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsAfter <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "kept the same post" 1 $ length postsAfter
      assertEq "kept the same post tag" 1 $ length postTagsAfter
      let Just postAfter' = listToMaybe postsAfter
      let Just postTag' = listToMaybe postTagsAfter

      assertEq "Post ids equal before and after" (entityKey post') (entityKey postAfter')
      assertEq "Post title" ((postTitle . entityVal) post') "The Post"
      assertEq "PostAfter title" ((postTitle . entityVal) postAfter') "This is the title"
      assertEq "Post content" ((Markdown.unMarkdown . postContent . entityVal) post') "First\nSecond\nThird"
      assertEq "PostAfter content" ((Markdown.unMarkdown . postContent . entityVal) postAfter') "## This is the content"
      assertEq "Post user" ((postUserId . entityVal) post') (entityKey user)
      assertEq "PostAfter user" ((postUserId . entityVal) postAfter') (entityKey user)
      assertEq "Post slug" ((Slug.unSlug . postSlug . entityVal) post') "the-slug"
      assertEq "PostAfter slug" ((Slug.unSlug . postSlug . entityVal) postAfter') "new-slug"
      assertEq "Post published" ((postPublished . entityVal) post') True
      assertEq "PostAfter published" ((postPublished . entityVal) postAfter') False

      assertEq "PostTag post id" ((postTagPostId . entityVal) postTag') (entityKey post')
      assertEq "PostTag tag id" ((postTagTagId . entityVal) postTag') (entityKey tag)
      bodyContains "Successfully saved draft"

    it "Should let you delete a post successfully" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"
      tag <- createTag "foo"
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-slug"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True
      _ <- createPostTag post' tag

      goToPost (EditPostR (entityKey post')) em "mypassword"

      postsBefore <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsBefore <- runDB $ selectList ([] :: [Filter PostTag]) []
      assertEq "Single post before we publish" 1 $ length postsBefore
      assertEq "Single post tag before we publish" 1 $ length postTagsBefore

      request $ do
        addToken
        addPostParam "action" "Delete"
        setMethod "POST"
        setUrl $ EditPostR (entityKey post')

      statusIs 303
      _ <- followRedirect
      statusIs 200

      postsAfter <- runDB $ selectList ([] :: [Filter Post]) []
      postTagsAfter <- runDB $ selectList ([] :: [Filter PostTag]) []

      assertEq "deleted the post" 0 $ length postsAfter
      assertEq "deleted the post tag" 0 $ length postTagsAfter
  where
    goToPost route em pass = do
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
      get route
      statusIs 200
