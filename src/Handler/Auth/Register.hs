{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Auth.Register where

import Import
import Helpers.Forms
import Helpers.Database
import Helpers.BCrypt

getRegisterR :: Handler Html
getRegisterR = do
  (formWidget, _) <- generateFormPost registerForm
  renderRegister formWidget []

postRegisterR :: Handler Html
postRegisterR = do
  ((result, formWidget), _) <- runFormPost registerForm
  case result of
    FormSuccess (email, password) -> do
      uid <- insertUser (User email Nothing)
      password' <- liftIO $ Password uid <$> hashPassword password
      _ <- insertPassword password'
      redirect LoginR
    _ -> do
      renderRegister formWidget ["Form failed validation"]

registerForm :: Form (Text, Text)
registerForm = renderDivs $ (,)
  <$> areq emailField emailSettings Nothing
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

renderRegister :: Widget -> [Text] -> Handler Html
renderRegister widget errors =
  defaultLayout $ do
    setTitle "Login"
    renderPanel $ [whamlet|
      <div>
        ^{formErrorWidget errors}
      <div>
        <form method="POST" action="@{RegisterR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Login">
    |]