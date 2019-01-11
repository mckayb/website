{-# LANGUAGE QuasiQuotes #-}

module Handler.Tag where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Helpers.Forms (FormReaction, FormAlert(Danger, Success))
import qualified Helpers.Forms as Forms
import qualified Helpers.Database as Database

getTagR :: Handler Html
getTagR = do
  (formWidget, _) <- generateFormPost tagForm
  renderTag formWidget []

postTagR :: Handler Html
postTagR = do
  ((result, formWidget), _) <- runFormPost tagForm
  case result of
    FormSuccess name -> do
      existingTag <- Database.getTagByName name
      case existingTag of
        Just _ -> renderTag formWidget [(Danger, "A tag already exists with that name")]
        Nothing -> do
          _ <- runDB $ insertEntity $ Tag name
          renderTag formWidget [(Success, "Successfully created new tag " <> name)]
    _ -> renderTag formWidget [(Danger, "Form failed validation")]

tagForm :: Form Text
tagForm = renderBootstrap3 BootstrapBasicForm $
  areq textField roleSettings Nothing
  where roleSettings = FieldSettings
          { fsLabel = "Tag"
          , fsId = Nothing
          , fsTooltip = Nothing
          , fsName = Nothing
          , fsAttrs = [("class", "form-control"), ("placeholder", "Tag Name")]
          }

renderTag :: Widget -> [FormReaction] -> Handler Html
renderTag widget errors =
  defaultLayout $ do
    setTitle "Structured Rants - New Tag"
    Forms.renderPanel "Create Tag" [whamlet|
      <div>
        ^{Forms.formReactionWidget errors}
      <div>
        <form method="POST" action="@{TagR}">
          ^{widget}
          <input .btn.btn-primary type="submit" value="Submit">
    |]
