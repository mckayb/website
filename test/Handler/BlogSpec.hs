module Handler.BlogSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email
import qualified Helpers.Slug as Slug
import qualified Helpers.Markdown as Markdown

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
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-test"
      let Right markdown = Markdown.mkMarkdown "## Test"

      user <- createUser role em
      _ <- createPost user "This is the title" markdown slug time True

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
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      let Right slug1 = Slug.mkSlug "first-i-was-afraid"
      let Right slug2 = Slug.mkSlug "i-was-petrified"
      let Right slug3 = Slug.mkSlug "could-never-live-without-you"
      let Right markdown1 = Markdown.mkMarkdown "First, I was afraid"
      let Right markdown2 = Markdown.mkMarkdown "I was petrified"
      let Right markdown3 = Markdown.mkMarkdown "Kept thinking I could never live without you by my side"
      _ <- createPost user "The First Post" markdown1 slug1 time True
      _ <- createPost user "The Second Post" markdown2 slug2 time True
      _ <- createPost user "The Third Post" markdown3 slug3 time True

      get BlogR
      statusIs 200
      htmlCount "article.post" 3
      htmlCount "article.post div.post__date" 3
      htmlAnyContain "article.post .post__title > a" "First"
      htmlAnyContain "article.post .post__title > a" "Second"
      htmlAnyContain "article.post .post__title > a" "Third"

    it "Only grabs the first paragraph from the post content" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\n\nSecond\n\nThird"
      _ <- createPost user "The Post" markdown slug time True

      get BlogR
      statusIs 200
      htmlAllContain ".post__title > a" "The Post"
      bodyContains "First"
      bodyNotContains "Second"

    it "Won't render a draft" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\n\nSecond\n\nThird"
      _ <- createPost user "The Post" markdown slug time False

      get BlogR
      statusIs 200
      bodyContains "Coming soon"

  describe "BlogPostSlugR" $ do
    it "Renders the post correctly if the post exists" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time True

      get $ BlogPostSlugR slug
      statusIs 200
      htmlCount "article.blog-post" 1
      htmlAllContain "h1" $ (unpack . postTitle . entityVal) post'
      bodyContains $ (unpack . Markdown.unMarkdown . postContent . entityVal) post'

    it "Renders not found if the post doesn't exist" $ do
      let Right slug = Slug.mkSlug "first-second-third"
      get $ BlogPostSlugR slug
      statusIs 404

    it "Won't let you visit a draft post if you aren't logged in" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      _ <- createPost user "The Post" markdown slug time False

      get $ BlogPostSlugR slug
      statusIs 403
      bodyContains "Permission Denied"
      bodyContains "You must login to access this page"

    it "Won't let you visit a draft post if you aren't an admin" $ do
      role <- createRole "Foo"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      _ <- createPost user "The Post" markdown slug time False

      authGet em "mypassword" $ BlogPostSlugR slug
      statusIs 403
      bodyContains "Permission Denied"
      bodyContains "You must be an admin to access this page"

  describe "BlogDraftsR" $ do
    it "Won't load if you aren't an admin" $ do
      get BlogDraftsR
      statusIs 403

    it "Renders no drafts" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" BlogDraftsR
      statusIs 200
      bodyContains "No drafts."

    it "Renders a single draft" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time False

      authGet em "mypassword" BlogDraftsR
      statusIs 200

      htmlAllContain ".post__title > a" $ (unpack . postTitle . entityVal) post'

    it "Renders multiple drafts" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first"
      let Right slug2 = Slug.mkSlug "second"
      let Right slug3 = Slug.mkSlug "third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time False
      post2' <- createPost user "The Second Post" markdown slug2 time False
      post3' <- createPost user "The Third Post" markdown slug3 time False

      authGet em "mypassword" BlogDraftsR
      statusIs 200

      htmlAnyContain ".post__title > a" $ (unpack . postTitle . entityVal) post'
      htmlAnyContain ".post__title > a" $ (unpack . postTitle . entityVal) post2'
      htmlAnyContain ".post__title > a" $ (unpack . postTitle . entityVal) post3'

    it "Lets you edit those drafts" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      user <- createUser role em
      _ <- createPassword user "mypassword"
      let Right slug = Slug.mkSlug "first-second-third"
      let Right markdown = Markdown.mkMarkdown "First\nSecond\nThird"
      post' <- createPost user "The Post" markdown slug time False

      authGet em "mypassword" BlogDraftsR
      statusIs 200

      htmlAllContain ".post__title > a" $ (unpack . postTitle . entityVal) post'
