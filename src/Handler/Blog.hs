{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Blog where

import Import
import Helpers.Database
import CMarkGFM
import Database.Persist.Sql
import qualified Data.Text as T

getPostContent :: Entity Post -> Text
getPostContent = (commonmarkToHtml [optSafe] []) . postContent . entityVal

getTimestamp :: Entity Post -> Text
getTimestamp = T.pack . formatTime defaultTimeLocale "%d %B %Y" . postTimestamp . entityVal

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

getBlogPostR :: Int64 -> Handler Html
getBlogPostR a = do
  post <- getPost a
  case post of
    Just post' -> do
      commentRoots <- getCommentsForPost post'
      liftIO $ print commentRoots
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
              $forall commentRoot <- commentRoots
                #{(commentMessage . entityVal) commentRoot}
        |]
    Nothing -> notFound