{-# LANGUAGE QuasiQuotes #-}

module Handler.Auth.Login where

import Import hiding (exp)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout(BootstrapBasicForm), renderBootstrap3)
import Helpers.Forms (FormReaction, FormAlert(Danger))
import Helpers.Email (Email)
import qualified Helpers.Database as Database
import qualified Helpers.BCrypt as BCrypt
import qualified Helpers.Forms as Forms
import qualified Helpers.Session as Session
import qualified Helpers.Email as Email

getLoginR :: Handler Html
getLoginR = do
  (formWidget, _) <- generateFormPost loginForm
  renderLogin formWidget []

postLoginR :: Handler Html
postLoginR = do
  ((result, formWidget), _) <- runFormPost loginForm
  case result of
  -- Validate the Form
    FormSuccess (email, password) -> do
      mUser <- Database.getUserByEmail email
      -- Validate that there's a user with that email
      case mUser of
        Just user' -> do
          -- Validate that there's a password associated with that user
          mPassword <- Database.getPasswordByUser user'
          case mPassword of
            Just (Entity _ password') -> do
              -- Validate that the password we have matches the password they gave
              let matches = BCrypt.passwordMatches (passwordHash password') password
              if matches
                then do
                  -- Grab the user's role so that we can do admin checks later
                  mRole <- Database.getRoleByUser user'
                  case mRole of
                    Just role -> do
                      Session.keepLoggedIn user' role
                      redirect BlogR
                    Nothing -> renderLogin formWidget [(Danger, "Something went wrong...")]
                else renderLogin formWidget [(Danger, "Incorrect username or password")]
            Nothing -> renderLogin formWidget [(Danger, "Incorrect username or password")]
        Nothing -> renderLogin formWidget [(Danger, "Incorrect username or password")]
    _ -> renderLogin formWidget [(Danger, "Form failed validation")]

loginForm :: Form (Email, Text)
loginForm = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq Email.emailField' emailSettings Nothing
  <*> areq passwordField passwordSettings Nothing
  where
    emailSettings = FieldSettings
      { fsLabel = "Email"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Email Address")]
      }
    passwordSettings = FieldSettings
      { fsLabel = "Password"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Password")]
      }

renderLogin :: Widget -> [FormReaction] -> Handler Html
renderLogin widget reactions =
  defaultLayout $ do
    setTitle "Structured Rants - Login"
    Forms.renderPanel "Login" [whamlet|
      <div>
        ^{Forms.formReactionWidget reactions}
      <div>
        <form method="POST" action="@{LoginR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Login">
    |]



