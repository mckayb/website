{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Post where

import Import
import CMarkGFM
import Helpers.Forms
import Helpers.Database

import Data.Aeson (decode)
import Data.String.Conversions (cs)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)

getPostR :: Handler Html
getPostR = do
  (formWidget, _) <- generateFormPost postForm
  renderPost formWidget Nothing []

postPostR :: Handler Html
postPostR = do
  ((result, formWidget), _) <- runFormPost postForm
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess (Textarea markdown), Just "Preview") -> do
      renderPost formWidget (Just $ previewWidget markdown) []
    (FormSuccess (Textarea markdown), Just "Publish") -> do
      mUserJson <- lookupSession userSessionKey
      let mUser = decode =<< cs <$> mUserJson :: Maybe (Entity User)
      case mUser of
        Just u -> do
          time <- liftIO getCurrentTime
          _ <- insertPost $ Post markdown time (entityKey u)
          renderPost formWidget Nothing []
        Nothing -> do
          renderPost formWidget Nothing ["Something went wrong."]
    _ -> do
      renderPost formWidget Nothing ["Something went wrong"]

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

previewWidget :: Text -> Widget
previewWidget txt = do
  let html = commonmarkToHtml [optSafe] [] txt
   in [whamlet|
        <div>
          #{preEscapedToMarkup html}
      |]

renderPost :: Widget -> Maybe Widget -> [Text] -> Handler Html
renderPost widget mPrev errors =
  defaultLayout $ do
    setTitle "Publish New Post"
    renderPanel $ do
      toWidget [lucius|
        section.post h4 {
          margin-bottom: 20px;
        }

        section.post .post__preview {
          border: 2px solid lightgray;
          padding: 20px;
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
          <h4>Create Post

          $if not (null errors)
            <div>
              ^{formErrorWidget errors}
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

