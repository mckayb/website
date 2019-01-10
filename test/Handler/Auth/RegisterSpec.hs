module Handler.Auth.RegisterSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker
import Helpers.Email
import Helpers.BCrypt

spec :: Spec
spec = withApp $ do
  describe "RegisterR" $ do
    it "Should render the register form correctly" $ do
      get RegisterR

      statusIs 200
      htmlCount ".alert.alert-danger" 0
      htmlCount "input[placeholder=Email]" 1
      htmlCount "input[placeholder=Password]" 1
      htmlCount "input.btn.btn-primary[type=submit][value=Register]" 1
    
    it "Should fail validation if there's no csrf token" $ do
      get RegisterR
      statusIs 200

      request $ do
        setMethod "POST"
        setUrl RegisterR

      statusIs 403

    it "Shouldn't let you register with invalid information" $ do
      get RegisterR
      statusIs 200

      request $ do
        addToken
        setMethod "POST"
        setUrl RegisterR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Form failed validation"

    it "Shouldn't let you register if you have a malformed email address" $ do
      get RegisterR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" "foo.bar.com"
        byLabelExact "Password" "aPassword"
        setMethod "POST"
        setUrl RegisterR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Form failed validation"

    it "Shouldn't let you register if you don't put in a password" $ do
      get RegisterR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" "foo.bar@examplecom"
        byLabelExact "Password" ""
        setMethod "POST"
        setUrl RegisterR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Form failed validation"

    it "Should register successfully with valid information" $ do
      Just em <- liftIO $ mkEmail . pack <$> Faker.email
      _ <- createRole "Admin"
      role2 <- createRole "Commoner"

      usersBefore <- runDB $ selectList ([] :: [Filter User]) []
      assertEq "user table empty" 0 $ length usersBefore
      passwordsBefore <- runDB $ selectList ([] :: [Filter Password]) []
      assertEq "password table empty" 0 $ length passwordsBefore

      get RegisterR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" $ unEmail em
        byLabelExact "Password" "my_password"
        setMethod "POST"
        setUrl RegisterR

      statusIs 303
      htmlCount ".alert.alert-danger" 0
      bodyNotContains "Form failed validation"

      usersAfter <- runDB $ selectList ([] :: [Filter User]) []
      assertEq "added a new user" 1 $ length usersAfter
      let Just user = listToMaybe usersAfter
      let entityUserEmail = (userEmail . entityVal) user
      let entityUserRole = (userRoleId . entityVal) user
      assertEq "email matches" entityUserEmail em
      assertEq "role matches" entityUserRole (entityKey role2)
      passwordsAfter <- runDB $ selectList ([] :: [Filter Password]) []
      assertEq "added a new password" 1 $ length passwordsAfter
      let Just pass = listToMaybe passwordsAfter
      let entityPasswordHash = (passwordHash . entityVal) pass
      let entityPasswordUser = (passwordUserId . entityVal) pass
      assertEq "password user matches" entityPasswordUser (entityKey user)
      assertEq "password hash matches" True (passwordMatches entityPasswordHash "my_password")

    it "Shouldn't let you register with a duplicate email" $ do
      role <- createRole "Admin"
      Just em <- liftIO $ mkEmail . pack <$> Faker.email
      user <- createUser role em
      _ <- createPassword user "my_password"

      get RegisterR
      statusIs 200

      request $ do
        addToken
        byLabelExact "Email" $ unEmail em
        byLabelExact "Password" "my_password"
        setMethod "POST"
        setUrl RegisterR

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "An account with that email already exists"



