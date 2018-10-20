{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Blog where

import Import hiding (insert, unionWithKey, singleton)
import Helpers.Database
import CMarkGFM (commonmarkToHtml, optSafe)
import Database.Persist.Sql (fromSqlKey)
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

      .row-bottom-border {
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
          <div .row .row-bottom-border>
            <div .col-md-12>
              <article .post data-post=#{getId post}>
                <span .post__time .text-muted>#{getTimestamp post}
                #{preEscapedToMarkup (getPostContent post)}
    |]

data Tree a = Nil | Node { treeLabel :: a, treeChildren :: [Tree a]}
  deriving (Eq, Show)

commentsToTree :: [Entity Comment] -> [Tree (Entity Comment)]
commentsToTree = foldl' accTree []
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

commentTreeWidget :: [Tree (Entity Comment)] -> Widget
commentTreeWidget comments = do
  toWidget [lucius|
    .blog-post ul.blog-post__comment {
      border-left: 1px solid gray;
      list-style: none;
      margin-bottom: 25px;
      margin-top: 25px;
    }
  |]
  [whamlet|
    $forall Node comment commentChildren <- comments
      <ul .blog-post__comment>
        <li>
          #{entityCommentMessage comment}
          ^{commentTreeWidget commentChildren}
          <div>
            <button>Reply
  |]
  where
    entityCommentMessage = commentMessage . entityVal

getBlogPostR :: Int64 -> Handler Html
getBlogPostR a = do
  post <- getPost a
  case post of
    Just post' -> do
      comments <- commentsToTree <$> getCommentsForPost post'
      defaultLayout $ do
        toWidget [lucius|
          .blog-post .blog-post__time {
            position: absolute;
            top: 0;
            right: 0;
          }
        |]
        [whamlet|
          <article .blog-post>
            <span .blog-post__time .text-muted>#{getTimestamp post'}
            #{preEscapedToMarkup (getPostContent post')}

            <hr>

            <div .blog-post__comments>
              ^{commentTreeWidget comments}
        |]
    Nothing -> notFound