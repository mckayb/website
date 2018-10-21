{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Blog where

import Import
import Helpers.Database
import Helpers.Forms
import CMarkGFM (commonmarkToHtml, optSafe)
import Database.Persist.Sql (fromSqlKey)
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Data.String.Conversions
import Data.Aeson
import qualified Data.Text as Text

getPostContent :: Entity Post -> Text
getPostContent = (commonmarkToHtml [optSafe] []) . postContent . entityVal

getTimestamp :: Entity Post -> Text
getTimestamp = Text.pack . formatTime defaultTimeLocale "%d %B %Y" . postTimestamp . entityVal

getBlogR :: Handler Html
getBlogR = do
  posts <- getPosts
  let getId = fromSqlKey . entityKey
  defaultLayout $ do
    setTitle "Blog"
    toWidget [lucius|
      .post:before {
        content:'';
        width:100%;
        height:100%;    
        position:absolute;
        left:0;
        top:0;
        background:linear-gradient(transparent 130px, white);
      }

      .post {
        max-height: 200px;
        overflow: hidden;
        margin-bottom: 21px;
        cursor: pointer;
      }

      .post .post__time {
        position: absolute;
        top: 20px;
        right: 20px;
      }

      .row--bottom-border {
        border-bottom: 1px solid lightgray;
        margin-left: 0;
        margin-right: 0;
        margin-bottom: 21px;
      }
    |]
    toWidget [julius|
      $(function() {
        $(".post").click(function() {
          document.location = "@{BlogR}/" + $(this).data("post")
        })
      })
    |]
    [whamlet|
      $if (null posts)
        <div>Coming soon!

      $else
        $forall post <- posts
          <div .row .row--bottom-border>
            <div .col-md-12>
              <article .post data-post=#{getId post}>
                <span .post__time .text-muted>#{getTimestamp post}
                #{preEscapedToMarkup (getPostContent post)}
    |]

data Tree a = Nil | Node { treeLabel :: a, treeChildren :: [Tree a]}
  deriving (Eq, Show)

commentsToTree :: [Entity Comment] -> [Tree (Entity Comment)]
commentsToTree = foldl' accTree mempty
  where
    recurseChildren :: [Tree (Entity Comment)] -> Key Comment -> Entity Comment -> [Tree (Entity Comment)]
    recurseChildren children parentKey comment = fmap (\t ->
      if (entityKey . treeLabel) t == parentKey
        then Node (treeLabel t) (treeChildren t ++ [Node comment []])
        else Node (treeLabel t) (recurseChildren (treeChildren t) parentKey comment)) children

    accTree :: [Tree (Entity Comment)] -> Entity Comment -> [Tree (Entity Comment)]
    accTree acc comment =
      case (commentParentId . entityVal) comment of
        Just parentKey -> recurseChildren acc parentKey comment
        Nothing -> acc ++ [Node comment []]

commentTreeWidget :: Key Post -> [Tree (Entity Comment)] -> Widget
commentTreeWidget postId comments =
  [whamlet|
    $forall Node comment commentChildren <- comments
      <ul .comment>
        <li>
          #{entityCommentMessage comment}
          ^{commentTreeWidget postId commentChildren}
          <input .btn.btn-link.btn-xs.comment__action type="submit" name="action" value="Reply">
  |]
  where
    entityCommentMessage = commentMessage . entityVal

rootCommentForm :: Form Textarea
rootCommentForm = renderBootstrap3 BootstrapBasicForm $
  areq textareaField messageSettings Nothing
  where 
    messageSettings = FieldSettings
      { fsLabel = "Comment"
      , fsId = Nothing
      , fsTooltip = Nothing
      , fsName = Nothing
      , fsAttrs = [("class", "form-control"), ("placeholder", "Comment Message")]
      }

getBlogPostR :: Key Post -> Handler Html
getBlogPostR = renderBlogPostR []

renderBlogPostR :: [FormReaction] -> Key Post -> Handler Html
renderBlogPostR reactions a = do
  post <- getPost a
  case post of
    Just post' -> do
      comments <- commentsToTree <$> getCommentsForPost post'
      (rootCommentFormWidget, _) <- generateFormPost rootCommentForm
      defaultLayout $ do
        toWidget [lucius|
          .blog-post .blog-post__time {
            position: absolute;
            top: 0;
            right: 15px;
          }

          .comment {
            border-left: 1px solid lightgray;
            list-style: none;
            margin-bottom: 25px;
            margin-top: 25px;
          }
        |]
        [whamlet|
          <article .blog-post>
            <span .blog-post__time .text-muted>#{getTimestamp post'}
            #{preEscapedToMarkup (getPostContent post')}

          <hr>

          <div>
            ^{formReactionWidget reactions}
          <div>
          $if not (null comments)
            <form method="POST" action="@{BlogPostR a}">
              <div .comments>
                ^{commentTreeWidget a comments}

          <form method="POST" action="@{BlogPostR a}">
            ^{rootCommentFormWidget}
            <input .btn.btn-primary type="submit" name="action" value="Submit">
        |]
    Nothing -> notFound

postBlogPostR :: Key Post -> Handler Html
postBlogPostR a = do
  user <- requireUser
  ((result, _), _) <- runFormPost rootCommentForm
  action <- lookupPostParam "action"
  liftIO $ putStrLn "\n\n\n\n"
  liftIO $ print result
  liftIO $ print action
  liftIO $ putStrLn "\n\n\n\n"
  case (result, action) of
    (FormSuccess (Textarea comment), Just "Submit") -> do
      time <- liftIO getCurrentTime
      _ <- runDB $ insertEntity $ Comment comment time (entityKey user) a Nothing
      renderBlogPostR [] a
    _ -> renderBlogPostR [(Danger, "Something went wrong!")] a

postBlogPostCommentR :: Key Post -> Key Comment -> Handler Html
postBlogPostCommentR a b = do
  user <- requireUser
  renderBlogPostR [] a

requireUser :: Handler (Entity User)
requireUser = do
  mUserJson <- lookupSession userSessionKey
  let mUser = decode =<< cs <$> mUserJson :: Maybe (Entity User)
  case mUser of
    Just u -> return u
    Nothing -> redirect LoginR