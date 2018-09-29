{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Post where

import Import
import CMarkGFM
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

import Helpers.Forms

getPostR :: Handler Html
getPostR = do
  (formWidget, _) <- generateFormPost postForm
  renderArticle formWidget Nothing []

postPostR :: Handler Html
postPostR = do
  ((result, formWidget), _) <- runFormPost postForm
  action <- lookupPostParam "action"
  liftIO $ print result
  case (result, action) of
    (FormSuccess (Textarea markdown), Just "Preview") -> do
      renderArticle formWidget (Just $ previewWidget markdown) ["Gonna need to preview this shit"]
    (FormSuccess _, Just "Publish") -> do
      renderArticle formWidget Nothing ["Gonna need to publish this shit"]
    _ -> do
      renderArticle formWidget Nothing ["Something went wrong"]

postForm :: Form (Textarea)
postForm = renderBootstrap3 BootstrapBasicForm $
  areq textareaField contentSettings Nothing
  where
    contentSettings = FieldSettings
      { fsLabel = "Content"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Content")]
      }

previewWidget :: Text -> Text
previewWidget txt = commonmarkToHtml [optSafe] [] txt

renderArticle :: Widget -> Maybe Text -> [Text] -> Handler Html
renderArticle widget mPrev errors =
  defaultLayout $ do
    setTitle "Publish New Post"
    renderPanel $ [whamlet|
      <div>
        ^{formErrorWidget errors}
      <div>
        $maybe prev <- mPrev
          <div>
            <h3>Preview
            <div .preview style="border: 2px solid lightgray; padding: 20px">
                #{preEscapedToMarkup prev}

        <br>

        <form method="POST" action="@{PostR}">
          ^{widget}
          <input .btn.btn-default type="submit" name="action" value="Preview">
          <input .btn.btn-primary type="submit" name="action" value="Publish">
    |]

