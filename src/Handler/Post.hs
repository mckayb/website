{-# LANGUAGE QuasiQuotes #-}

module Handler.Post where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Helpers.Forms (FormReaction, FormAlert(Danger, Success))
import Helpers.Slug (Slug, slugField)
import Helpers.Markdown (Markdown(Markdown), markdownField)
import qualified Helpers.Forms as Forms
import qualified Helpers.Session as Session
import qualified Helpers.Theme as Theme
import qualified Helpers.Database as Database
import qualified Text.MMark as MMark
import qualified Text.MMark.Extension.Common as Ext
import qualified Lucid.Base as Lucid

getTagOpts :: Handler [(Text, Key Tag)]
getTagOpts = fmap (\tag -> ((tagName . entityVal) tag, entityKey tag)) <$> Database.getTags

getPostR :: Handler Html
getPostR = do
  _ <- Session.requireAdminUser
  opts <- getTagOpts
  (formWidget, _) <- generateFormPost $ postForm opts
  renderPost formWidget Nothing []

postPostR :: Handler Html
postPostR = do
  user <- Session.requireAdminUser
  opts <- getTagOpts
  ((result, formWidget), _) <- runFormPost $ postForm opts
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess (title, Markdown markdown, _, _), Just "Preview") ->
      renderPost formWidget (Just $ previewWidget title markdown) []
    (FormSuccess (title, markdown, slug, tagIds), Just "Publish") -> do
      time <- liftIO getCurrentTime
      post <- runDB $ insertEntity $ Post title markdown slug time (entityKey user)
      _ <- runDB $ insertMany $ fmap (PostTag (entityKey post)) tagIds
      renderPost formWidget Nothing [(Success, "Successfully published new post")]
    _ -> do
      renderPost formWidget Nothing [(Danger, "Something went wrong")]

postForm :: [(Text, Key Tag)] -> Form (Text, Markdown, Slug, [Key Tag])
postForm opts =
  renderBootstrap3 BootstrapBasicForm $ (,,,)
  <$> areq textField titleSettings Nothing
  <*> areq markdownField contentSettings Nothing
  <*> areq slugField slugSettings Nothing
  <*> areq (multiSelectFieldList opts) multiSelectSettings Nothing
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
    slugSettings = FieldSettings
      { fsLabel = "Slug"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Slug")]
      }
    multiSelectSettings = FieldSettings
      { fsLabel = "Tags"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Tags")]
      }

previewWidget :: Text -> Text -> Widget
previewWidget title markdown =
  let render = preEscapedToMarkup . Lucid.renderText . MMark.render . MMark.useExtensions [Ext.ghcSyntaxHighlighter, Ext.skylighting]
  in case MMark.parse "" markdown of
    Left errs -> [whamlet|
      <h2>Errors
      <div>#{MMark.parseErrorsPretty markdown errs}
    |]
    Right r -> [whamlet|
      <h1>#{title}
      #{render r}
    |]

renderPost :: Widget -> Maybe Widget -> [FormReaction] -> Handler Html
renderPost widget mPrev reactions =
  defaultLayout $ do
    setTitle "Structured Rants - New Post"
    Forms.renderPanel "Create Post" $ do
      toWidget [lucius|
        section.post .post__preview {
          border-bottom: 1px solid #{Theme.borderColor Theme.colorScheme};
          padding: 2rem;
          border-radius: 5px;
        }
      |]
      [whamlet|
        <section .post>
          $if not (null reactions)
            <div>
              ^{Forms.formReactionWidget reactions}
            <br>

          $maybe prev <- mPrev
            <div .post__preview>
              ^{prev}
            <br>

          <div>
            <form method="POST" action="@{PostR}">
              ^{widget}
              <input .btn.btn-default type="submit" name="action" value="Preview">
              <input .btn.btn-primary type="submit" name="action" value="Publish">
      |]

