module Handler.BlogSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email
import qualified Helpers.Slug as Slug

spec :: Spec
spec = withApp $ do
  describe "BlogR" $ do
    it "Shows Coming Soon if there are not any posts" $ do
      get BlogR

      htmlCount "article.post" 0
      bodyContains "Coming soon!"
      statusIs 200

    it "Works if there is a single post" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-test"

      user <- createUser role em
      _ <- createPost user "This is the title" "## Test" slug time

      get BlogR
      statusIs 200
      htmlCount "article.post" 1
      htmlCount "article.post div.post__date" 1
      htmlAnyContain "article.post .post__title > a" "This is the title"
      htmlAnyContain "h2" "Test"

      htmlCount "article.blog-post" 0
      clickOn "article.post .post__title > a"
      statusIs 200
      htmlCount "article.blog-post" 1

    it "Works if there are multiple posts" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      let Right slug1 = Slug.mkSlug "first-i-was-afraid"
      let Right slug2 = Slug.mkSlug "i-was-petrified"
      let Right slug3 = Slug.mkSlug "could-never-live-without-you"
      _ <- createPost user "The First Post" "First, I was afraid" slug1 time
      _ <- createPost user "The Second Post" "I was petrified" slug2 time
      _ <- createPost user "The Third Post" "Kept thinking I could never live without you by my side" slug3 time

      get BlogR
      statusIs 200
      htmlCount "article.post" 3
      htmlCount "article.post div.post__date" 3
      htmlAnyContain "article.post .post__title > a" "First"
      htmlAnyContain "article.post .post__title > a" "Second"
      htmlAnyContain "article.post .post__title > a" "Third"

    it "Only grabs the first paragraph from the post content" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      let Right slug = Slug.mkSlug "first-second-third"
      _ <- createPost user "The Post" "First\n\nSecond\n\nThird" slug time

      get BlogR
      statusIs 200
      htmlAllContain ".post__title > a" "The Post"
      bodyContains "First"
      bodyNotContains "Second"

  describe "BlogPostSlugR" $ do
    it "Renders the post correctly if the post exists" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      let Right slug = Slug.mkSlug "first-second-third"
      post' <- createPost user "The Post" "First\nSecond\nThird" slug time

      get $ BlogPostSlugR slug
      statusIs 200
      htmlCount "article.blog-post" 1
      htmlAllContain "h1" $ (unpack . postTitle . entityVal) post'
      bodyContains $ (unpack . postContent . entityVal) post'

    it "Renders not found if the post doesn't exist" $ do
      let Right slug = Slug.mkSlug "first-second-third"
      get $ BlogPostSlugR slug
      statusIs 404
