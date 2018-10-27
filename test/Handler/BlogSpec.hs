{-# LANGUAGE NoImplicitPrelude #-}

module Handler.BlogSpec (spec) where

import TestImport
import Faker.Internet

spec :: Spec
spec = withApp $ do
  describe "getBlogR" $ do
    it "Shows Coming Soon if there are not any posts" $ do
      get BlogR

      htmlCount "article.post" 0
      bodyContains "Coming soon!"
      statusIs 200

    it "Works if there is a single post" $ do
      role <- createRole "Admin"
      em <- liftIO $ pack <$> email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPost user "This is the title" "## Test" time

      get BlogR
      statusIs 200
      htmlCount "article.post" 1
      htmlCount "article.post div.post__date.text-muted" 1
      htmlAnyContain "h1" "This is the title"
      htmlAnyContain "h2" "Test"

      htmlCount "article.blog-post" 0
      clickOn "article.post > h1.post__title > a"
      statusIs 200
      htmlCount "article.blog-post" 1

    it "Works if there are multiple posts" $ do
      role <- createRole "Admin"
      em <- liftIO $ pack <$> email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPost user "The First Post" "First, I was afraid" time
      _ <- createPost user "The Second Post" "I was petrified" time
      _ <- createPost user "The Third Post" "Kept thinking I could never live without you by my side" time

      get BlogR
      statusIs 200
      htmlCount "article.post" 3
      htmlCount "article.post div.post__date.text-muted" 3
      htmlAnyContain "h1" "First"
      htmlAnyContain "h1" "Second"
      htmlAnyContain "h1" "Third"

    it "Only grabs the first paragraph from the post content" $ do
      role <- createRole "Admin"
      em <- liftIO $ pack <$> email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPost user "The Post" "First\n\nSecond\n\nThird" time

      get BlogR
      statusIs 200
      htmlAllContain "h1.post__title > a" "The Post"
      bodyContains "First"
      bodyNotContains "Second"

