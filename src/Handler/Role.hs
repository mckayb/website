{-# LANGUAGE QuasiQuotes #-}

module Handler.Role where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Helpers.Forms (FormReaction, FormAlert(Danger, Success))
import qualified Helpers.Forms as Forms
import qualified Helpers.Database as Database

getRoleR :: Handler Html
getRoleR = do
  (formWidget, _) <- generateFormPost roleForm
  renderRole formWidget []

postRoleR :: Handler Html
postRoleR = do
  ((result, formWidget), _) <- runFormPost roleForm
  case result of
    FormSuccess name -> do
      existingRole <- Database.getRoleByName name
      case existingRole of
        Just _ -> renderRole formWidget [(Danger, "A role already exists with that name.")]
        Nothing -> do
          _ <- runDB $ insertEntity $ Role name
          renderRole formWidget [(Success, "Successfully created new role" <> name <> ".")]
    _ -> renderRole formWidget [(Danger, "There was an error submitting your form.")]

roleForm :: Form Text
roleForm = renderBootstrap3 BootstrapBasicForm $
  areq textField roleSettings Nothing
  where roleSettings = FieldSettings
          { fsLabel = "Role"
          , fsId = Nothing
          , fsTooltip = Nothing
          , fsName = Nothing
          , fsAttrs = [("class", "form-control"), ("placeholder", "Role")]
          }

renderRole :: Widget -> [FormReaction] -> Handler Html
renderRole widget errors =
  defaultLayout $ do
    setTitle "Create Role"
    Forms.renderPanel [whamlet|
      <div>
        ^{Forms.formReactionWidget errors}
      <div>
        <form method="POST" action="@{RoleR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Submit">
    |]
