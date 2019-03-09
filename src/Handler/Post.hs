{-# LANGUAGE QuasiQuotes #-}

module Handler.Post where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Helpers.Forms (FormReaction, FormAlert(Danger, Success))
import Helpers.Slug (Slug, slugField)
import Helpers.Markdown (Markdown, markdownField)
import qualified Helpers.Forms as Forms
import qualified Helpers.Session as Session
import qualified Helpers.Database as Database
import qualified Handler.Blog as Blog

getTagOpts :: Handler [(Text, Key Tag)]
getTagOpts = fmap (\tag -> ((tagName . entityVal) tag, entityKey tag)) <$> Database.getTags

getCreatePostR :: Handler Html
getCreatePostR = do
  _ <- Session.requireAdminUser
  opts <- getTagOpts
  (formWidget, _) <- generateFormPost $ postForm Nothing opts
  renderPost (createForm formWidget) Nothing []

postCreatePostR :: Handler Html
postCreatePostR = do
  user <- Session.requireAdminUser
  opts <- getTagOpts
  ((result, formWidget), _) <- runFormPost $ postForm Nothing opts
  action <- lookupPostParam "action"
  case (result, action) of
    (FormSuccess (title, markdown, slug, _), Just "Preview") -> do
      time <- liftIO getCurrentTime
      renderPost (createForm formWidget) (Just $ Blog.postWidget $ Post title markdown slug time (entityKey user) False) []

    (FormSuccess (title, markdown, slug, tagIds), Just "Save as Draft") -> do
      time <- liftIO getCurrentTime
      post <- runDB $ insertEntity $ Post title markdown slug time (entityKey user) False
      _ <- runDB $ insertMany $ fmap (PostTag (entityKey post)) tagIds
      renderPost (createForm formWidget) Nothing [(Success, "Successfully saved draft")]

    (FormSuccess (title, markdown, slug, tagIds), Just "Publish") -> do
      time <- liftIO getCurrentTime
      post <- runDB $ insertEntity $ Post title markdown slug time (entityKey user) True
      _ <- runDB $ insertMany $ fmap (PostTag (entityKey post)) tagIds
      renderPost (createForm formWidget) Nothing [(Success, "Successfully published new post")]

    _ -> renderPost (createForm formWidget) Nothing [(Danger, "Form failed validation")]

postForm :: Maybe (Entity Post, [Entity Tag]) -> [(Text, Key Tag)] -> Form (Text, Markdown, Slug, [Key Tag])
postForm pwt opts =
  renderBootstrap3 BootstrapBasicForm $ (,,,)
  <$> areq textField titleSettings maybeTitle
  <*> areq markdownField contentSettings maybeMarkdown
  <*> areq slugField slugSettings maybeSlug
  <*> areq (multiSelectFieldList opts) multiSelectSettings maybeTags
  where
    maybeTitle = fmap (postTitle . entityVal . fst) pwt
    maybeMarkdown = fmap (postContent . entityVal . fst) pwt
    maybeSlug = fmap (postSlug . entityVal . fst) pwt
    maybeTags = fmap (fmap entityKey . snd) pwt
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
      , fsAttrs = [("class", "form-control"), ("placeholder", "Content"), ("rows", "20")]
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

renderPost :: Widget -> Maybe Widget -> [FormReaction] -> Handler Html
renderPost form mPrev reactions =
  defaultLayout $ do
    setTitle "Structured Rants - New Post"
    Forms.renderPanel "Create Post" $ do
      toWidget [lucius|
        .post .post__preview {
          padding: 2rem;
        }
      |]
      [whamlet|
        <section .post>
          $if not (null reactions)
            <div>
              ^{Forms.formReactionWidget reactions}
            <br>

          $maybe prev <- mPrev
            <div .post__preview >
              ^{prev}
            <br>

          <div>
            ^{form}
      |]

createForm :: Widget -> Widget
createForm widget = [whamlet|
  <form method="POST" action="@{CreatePostR}">
    ^{widget}
    <input .btn.btn-default type="submit" name="action" value="Preview"/>
    <input .btn.btn-default type="submit" name="action" value="Save as Draft"/>
    <input .btn.btn-primary type="submit" name="action" value="Publish"/>
|]

editForm :: Key Post -> Widget -> Widget
editForm postId widget = [whamlet|
  <form method="POST" action="@{EditPostR postId}">
    ^{widget}
    <input .btn.btn-default type="submit" name="action" value="Preview"/>
    <input .btn.btn-default type="submit" name="action" value="Save as Draft"/>
    <input .btn.btn-primary type="submit" name="action" value="Revise and Publish"/>
    <input .btn.btn-danger type="submit" name="action" value="Delete" style="float: right"/>
|]

getEditPostR :: Key Post -> Handler Html
getEditPostR postId = do
  _ <- Session.requireAdminUser
  postWithTags <- Database.getPostWithTags postId
  case postWithTags of
    Just _ -> do
      opts <- getTagOpts
      (formWidget, _) <- generateFormPost $ postForm postWithTags opts
      renderPost (editForm postId formWidget) Nothing []
    Nothing -> notFound

postEditPostR :: Key Post -> Handler Html
postEditPostR postId = do
  user <- Session.requireAdminUser
  postWithTags <- Database.getPostWithTags postId
  case postWithTags of
    Just _ -> do
      opts <- getTagOpts
      ((result, formWidget), _) <- runFormPost $ postForm postWithTags opts
      action <- lookupPostParam "action"
      case (result, action) of
        (FormSuccess (title, markdown, slug, _), Just "Preview") -> do
          time <- liftIO getCurrentTime
          renderPost (editForm postId formWidget) (Just $ Blog.postWidget $ Post title markdown slug time (entityKey user) False) []

        (FormSuccess (title, markdown, slug, tagIds), Just "Save as Draft") -> do
          time <- liftIO getCurrentTime
          runDB $ replace postId $ Post title markdown slug time (entityKey user) False
          runDB $ deleteWhere [PostTagPostId ==. postId]
          _ <- runDB $ insertMany $ fmap (PostTag postId) tagIds
          renderPost (editForm postId formWidget) Nothing [(Success, "Successfully saved draft")]

        (FormSuccess (title, markdown, slug, tagIds), Just "Revise and Publish") -> do
          time <- liftIO getCurrentTime
          runDB $ replace postId $ Post title markdown slug time (entityKey user) True
          runDB $ deleteWhere [PostTagPostId ==. postId]
          _ <- runDB $ insertMany $ fmap (PostTag postId) tagIds

          renderPost (editForm postId formWidget) Nothing [(Success, "Successfully published new post")]

        (_, Just "Delete") -> do
          runDB $ deleteWhere [PostTagPostId ==. postId]
          runDB $ delete postId
          redirect BlogR

        _ -> renderPost (editForm postId formWidget) Nothing [(Danger, "Form failed validation")]
    Nothing -> notFound