{-# LANGUAGE QuasiQuotes #-}

module Handler.Auth.Register where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Helpers.Forms (FormReaction, FormAlert(Danger))
import Helpers.Email (Email)
import qualified Helpers.Forms as Forms
import qualified Helpers.Database as Database
import qualified Helpers.BCrypt as BCrypt
import qualified Helpers.Email as Email
import qualified Database.Persist.Sql as Sql (toSqlKey)

getRegisterR :: Handler Html
getRegisterR = do
  (formWidget, _) <- generateFormPost registerForm
  renderRegister formWidget []

postRegisterR :: Handler Html
postRegisterR = do
  ((result, formWidget), _) <- runFormPost registerForm
  case result of
    FormSuccess (email, password) -> do
      existingUser <- Database.getUserByEmail email
      case existingUser of
        Just _ -> renderRegister formWidget [(Danger, "An account with that email already exists!")]
        Nothing -> do
          uid <- Database.insertUser (User email (Sql.toSqlKey 2))
          password' <- liftIO $ Password uid <$> BCrypt.hashPassword password
          _ <- Database.insertPassword password'
          redirect LoginR
    _ -> do
      renderRegister formWidget [(Danger, "Form failed validation")]

registerForm :: Form (Email, Text)
registerForm = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq Email.emailField' emailSettings Nothing
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
    Forms.renderPanel $ [whamlet|
      <div>
        ^{Forms.formReactionWidget reactions}
      <div>
        <form method="POST" action="@{RegisterR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Register">
    |]