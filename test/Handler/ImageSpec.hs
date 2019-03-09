module Handler.ImageSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import qualified Helpers.Email as Email

spec :: Spec
spec = withApp $ do
  describe "ImageR" $ do
    it "Should not let you go to the page without a user that is currently logged in" $ do
      get ImageR
      statusIs 403

    it "Should let you visit the page if you are an admin" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" ImageR

      htmlCount ".alert.alert-danger" 0
      htmlCount "input[type=file]" 1
      htmlCount "input.btn.btn-primary[type=submit][value=Submit]" 1

    it "Should fail if there's no csrf token" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" ImageR

      request $ do
        setMethod "POST"
        setUrl ImageR

      statusIs 403


    it "Should return errors if you try to submit the form without uploading an image" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" ImageR

      request $ do
        addToken
        setMethod "POST"
        setUrl ImageR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Should let you successfully upload an image" $ do
      role <- createRole "Admin"
      em <- liftIO $ just . Email.mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      authGet em "mypassword" ImageR

      request $ do
        addToken
        setMethod "POST"
        setUrl ImageR
        fileByLabelExact "Image" "test/fixtures/8bit-mario.jpg" "image/jpeg"

      statusIs 200
      htmlCount ".alert.alert-danger" 0
      bodyContains "Successfully uploaded image: 8bit-mario.jpg"
