module Handler.RssSpec where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email
import qualified Helpers.Slug as Slug
import qualified Helpers.Markdown as Markdown

spec :: Spec
spec = withApp $ do
  describe "RssR" $ do
    it "Works if there are no posts" $ do
      get RssR

      statusIs 200
      bodyContains "Structured Rants"
      bodyContains "Musings about Tech, Math, Arcade Games or whatever else I decide to monologue about."

    it "Shows a single published post" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-test"
      let Right markdown = Markdown.mkMarkdown "## Test"
      user <- createUser role em
      _ <- createPost user "This is the title" markdown slug time True

      get RssR

      statusIs 200
      bodyContains "Structured Rants"
      bodyContains "Musings about Tech, Math, Arcade Games or whatever else I decide to monologue about."
      bodyContains "This is the title"
      bodyContains "Test"

    it "Does not show a draft post" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      time <- liftIO getCurrentTime
      let Right slug = Slug.mkSlug "the-test"
      let Right markdown = Markdown.mkMarkdown "## Test"
      user <- createUser role em
      _ <- createPost user "This is the title" markdown slug time False

      get RssR

      statusIs 200
      bodyContains "Structured Rants"
      bodyContains "Musings about Tech, Math, Arcade Games or whatever else I decide to monologue about."
      bodyNotContains "This is the title"
      bodyNotContains "Test"
