{-# LANGUAGE QuasiQuotes #-}

module Handler.Post where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Helpers.Forms (FormReaction, FormAlert(Danger))
import qualified CMarkGFM
import qualified Helpers.Forms as Forms
import qualified Helpers.Session as Session
import qualified Helpers.Theme as Theme

getPostR :: Handler Html
getPostR = do
  _ <- Session.requireAdminUser
  (formWidget, _) <- generateFormPost postForm
  renderPost formWidget Nothing []

postPostR :: Handler Html
postPostR = do
  user <- Session.requireAdminUser
  ((result, formWidget), _) <- runFormPost postForm
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess (title, Textarea markdown), Just "Preview") ->
      renderPost formWidget (Just $ previewWidget title markdown) []
    (FormSuccess (title, Textarea markdown), Just "Publish") -> do
      time <- liftIO getCurrentTime
      _ <- runDB $ insertEntity $ Post title markdown time (entityKey user)
      renderPost formWidget Nothing []
    _ -> renderPost formWidget Nothing [(Danger, "Something went wrong")]

postForm :: Form (Text, Textarea)
postForm = renderBootstrap3 BootstrapBasicForm $ (,)
  <$> areq textField titleSettings Nothing
  <*> areq textareaField contentSettings Nothing
  where
    titleSettings = FieldSettings
      { fsLabel = "Title"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Title")]
      }
    contentSettings = FieldSettings
      { fsLabel = "Content"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Content")]
      }

previewWidget :: Text -> Text -> Widget
previewWidget title markdown =
  let html = CMarkGFM.commonmarkToHtml [] [] markdown
   in [whamlet|
        <div>
          <h1>#{title}
          #{preEscapedToMarkup html}
      |]

renderPost :: Widget -> Maybe Widget -> [FormReaction] -> Handler Html
renderPost widget mPrev reactions =
  defaultLayout $ do
    setTitle "Publish New Post"
    Forms.renderPanel "Create Post" $ do
      toWidget [lucius|
        section.post .post__preview {
          border: 1px solid #{Theme.borderColor Theme.colorScheme};
          padding: 2rem;
          border-radius: 5px;
        }

        section.post .post__label {
          font-weight: bold;
          margin-bottom: 5px;
          max-width: 100%;
          display: inline-block;
        }
      |]
      [whamlet|
        <section .post>
          $if not (null reactions)
            <div>
              ^{Forms.formReactionWidget reactions}
            <br>

          $maybe prev <- mPrev
            <div .post__label>Preview
            <div .post__preview>
              ^{prev}
            <br>

          <div>
            <form method="POST" action="@{PostR}">
              ^{widget}
              <input .btn.btn-default type="submit" name="action" value="Preview">
              <input .btn.btn-primary type="submit" name="action" value="Publish">
      |]

