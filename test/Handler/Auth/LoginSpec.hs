module Handler.Auth.LoginSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import Helpers.Email

spec :: Spec
spec = withApp $ do
  describe "LoginR" $ do
    it "Should work correctly" $ do
      get LoginR

      statusIs 200
      htmlCount ".alert.alert-danger" 0
      htmlCount "input[placeholder=Email Address]" 1
      htmlCount "input[placeholder=Password]" 1
      htmlCount "input.btn.btn-primary[type=submit][value=Login]" 1

    it "Should fail if no csrf token is set" $ do
      get LoginR
      statusIs 200

      request $ do
        setMethod "POST"
        setUrl LoginR

      statusIs 403

    it "Should fail if no email or password is set" $ do
      get LoginR
      statusIs 200

      -- Post should fail other form validation
      request $ do
        addToken
        setMethod "POST"
        setUrl LoginR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Form failed validation"
      bodyContains "Value is required"

    it "Should not let you log in if you have a malformed email address" $ do
      get LoginR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" "foobar"
        byLabelExact "Password" "aPassword"
        setMethod "POST"
        setUrl LoginR

      statusIs 200
      bodyContains "Form failed validation"

    it "Should not let you log in if that email doesn't exist" $ do
      get LoginR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" "foo.bar@gmail.com"
        byLabelExact "Password" "aPassword"
        setMethod "POST"
        setUrl LoginR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Incorrect username or password"

    it "Should not let you log in if a password doesn't exist for the user" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ mkEmail . pack <$> Faker.email
      _ <- createUser role em

      get LoginR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" $ unEmail em
        byLabelExact "Password" "wrongpassword"
        setMethod "POST"
        setUrl LoginR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Incorrect username or password"

    it "Should not let you log in if you have the wrong password" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      get LoginR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" $ unEmail em
        byLabelExact "Password" "wrongpassword"
        setMethod "POST"
        setUrl LoginR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Incorrect username or password"

    it "Should let you log in successfully" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "mypassword"

      get LoginR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" $ unEmail em
        byLabelExact "Password" "mypassword"
        setMethod "POST"
        setUrl LoginR

      statusIs 303

      eitherUrl <- followRedirect
      statusIs 200

      assertEq "Redirect worked" (Right "/") eitherUrl
      htmlCount ".alert.alert-danger" 0
