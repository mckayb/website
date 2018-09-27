{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Auth where

import Import
import Web.JWT            (encodeSigned, decodeAndVerifySignature)
import API.Database
import API.BCrypt
import System.Environment (lookupEnv)

getLoginR :: Handler Html
getLoginR = do
  (formWidget, _) <- generateFormPost loginForm
  renderLogin formWidget []

postLoginR :: Handler Html
postLoginR = do
  ((result, formWidget), _) <- runFormPost loginForm
  liftIO $ print result
  case result of
    FormSuccess (email, password) -> do
      mUser <- getUserByEmail email
      case mUser of
        Just user' -> do
          mPassword <- getPasswordByUser user'
          _ <- case mPassword of
            Just (Entity _ password') -> do
              let matches = passwordMatches (passwordHash password') password
              case matches of
                False -> renderLogin formWidget ["Incorrect username or password"]
                True -> do
                  -- SET THE SESSION STUFF HERE
                  testShit <- liftIO $ lookupEnv "TEST_SHIT"
                  liftIO $ print testShit
                  redirect HomeR
            Nothing -> do
              renderLogin formWidget ["Incorrect username or password"]
          redirect HomeR
        Nothing -> do
          renderLogin formWidget ["Incorrect username or password"]
    _ -> do
      renderLogin formWidget ["Form failed validation"]

loginForm :: Form (Text, Text)
loginForm = renderDivs $ (,)
  <$> areq emailField "Email" Nothing
  <*> areq passwordField "Password" Nothing

formErrorWidget :: [Text] -> Widget
formErrorWidget formErrors = [whamlet|
$if not (null formErrors)
  $forall formError <- formErrors
    <div .alert.alert-danger>
      #{formError}
|]

renderPanel :: Widget -> Widget
renderPanel widget = [whamlet|
<div .panel>
  <div .panel-body>
    ^{widget}
|]

renderLogin :: Widget -> [Text] -> Handler Html
renderLogin widget errors =
  defaultLayout $ do
    setTitle "Login"
    renderPanel $ [whamlet|
      <div>
        ^{formErrorWidget errors}
      <div>
        <form method="POST" action="@{LoginR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Login">
    |]



