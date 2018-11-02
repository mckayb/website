{-# LANGUAGE QuasiQuotes #-}

module Handler.Auth.Login where

import Import hiding (exp)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Helpers.Database
import Helpers.BCrypt
import Helpers.Forms
import Helpers.Session
import Helpers.Email

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
      mUser <- getUserByEmail email
      -- Validate that there's a user with that email
      case mUser of
        Just user' -> do
          -- Validate that there's a password associated with that user
          mPassword <- getPasswordByUser user'
          case mPassword of
            Just (Entity _ password') -> do
              -- Validate that the password we have matches the password they gave
              let matches = passwordMatches (passwordHash password') password
              case matches of
                False -> renderLogin formWidget [(Danger, "Incorrect username or password")]
                True -> do
                  -- Grab the user's role so that we can do admin checks later
                  mRole <- getRoleByUser user'
                  case mRole of
                    Just role -> do
                      keepLoggedIn user' role
                      redirect HomeR
                    Nothing -> renderLogin formWidget [(Danger, "Something went wrong...")]
            Nothing -> do
              renderLogin formWidget [(Danger, "Incorrect username or password")]
        Nothing -> do
          renderLogin formWidget [(Danger, "Incorrect username or password")]
    _ -> do
      renderLogin formWidget [(Danger, "Form failed validation")]

loginForm :: Form (Email, Text)
loginForm = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq emailField' emailSettings Nothing
  <*> areq passwordField passwordSettings Nothing
  where
    emailSettings = FieldSettings
      { fsLabel = "Email"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Email")]
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
    setTitle "Login"
    renderPanel $ [whamlet|
      <div>
        ^{formReactionWidget reactions}
      <div>
        <form method="POST" action="@{LoginR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Login">
    |]



