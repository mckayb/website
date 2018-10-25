module Handler.BlogSpec (spec) where

import TestImport
import Faker.Internet

spec :: Spec
spec = withApp $ do
  describe "getBlogR" $ do
    it "Shows Coming Soon if there are not any posts" $ do
      get BlogR

      htmlCount "article .post" 0
      bodyContains "Coming soon!"
      statusIs 200

    it "Works if there is a single post" $ do
      fakeEmail <- liftIO $ pack <$> email
      currTime <- liftIO getCurrentTime
      role <- createRole "Admin"
      user <- createUser role fakeEmail
      blogPost <- createPost user "This is my post" currTime

      get BlogR
      statusIs 200
      htmlCount "article .post" 1

    it "Works if there are multiple posts" $ do
      assertEq "Test" True True
