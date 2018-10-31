{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Auth.Register where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Helpers.Forms
import Helpers.Database
import Helpers.BCrypt
import Helpers.Email
import Database.Persist.Sql

getRegisterR :: Handler Html
getRegisterR = do
  (formWidget, _) <- generateFormPost registerForm
  renderRegister formWidget []

postRegisterR :: Handler Html
postRegisterR = do
  ((result, formWidget), _) <- runFormPost registerForm
  case result of
    FormSuccess (email, password) -> do
      uid <- insertUser (User email (toSqlKey 2))
      password' <- liftIO $ Password uid <$> hashPassword password
      _ <- insertPassword password'
      redirect LoginR
    _ -> do
      renderRegister formWidget [(Danger, "Form failed validation")]

registerForm :: Form (Email, Text)
registerForm = renderBootstrap3 BootstrapBasicForm $ (,)
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

renderRegister :: Widget -> [FormReaction] -> Handler Html
renderRegister widget reactions =
  defaultLayout $ do
    setTitle "Register"
    renderPanel $ [whamlet|
      <div>
        ^{formReactionWidget reactions}
      <div>
        <form method="POST" action="@{RegisterR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Register">
    |]