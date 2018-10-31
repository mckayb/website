module Handler.Auth.RegisterSpec (spec) where

import TestImport
import qualified Faker.Internet as Faker

spec :: Spec
spec = withApp $ do
  describe "Register Form" $ do
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

      statusIs 200
      htmlCount ".alert.alert-danger" 1
      bodyContains "Form failed validation"

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

