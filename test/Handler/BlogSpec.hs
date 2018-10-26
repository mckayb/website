{-# LANGUAGE NoImplicitPrelude #-}

module Handler.BlogSpec (spec) where

import TestImport
import Faker.Internet
import Database.Persist.Sql (fromSqlKey)

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
      post' <- createPost user "# Test" time
      let idSel = pack $ "#post_" <> (show . fromSqlKey $ entityKey post')

      get BlogR
      statusIs 200
      htmlCount "article.post" 1
      htmlCount "span.text-muted" 1

      htmlAnyContain "h1" "Test"
      htmlCount idSel 1

      -- clickOn idSel
      -- statusIs 200
      -- htmlCount "article.blog-post" 1

    it "Works if there are multiple posts" $ do
      role <- createRole "Admin"
      em <- liftIO $ pack <$> email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPost user "# First" time
      _ <- createPost user "# Second" time
      _ <- createPost user "# Third" time

      get BlogR
      statusIs 200
      htmlCount "article.post" 3
      htmlCount "span.text-muted" 3
      htmlAnyContain "h1" "First"
      htmlAnyContain "h1" "Second"
      htmlAnyContain "h1" "Third"
