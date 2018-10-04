{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Blog where

import Import
import Helpers.Database
import CMarkGFM
import Database.Persist.Sql

getBlogR :: Handler Html
getBlogR = do
  posts <- getPosts
  let getHtml = (commonmarkToHtml [optSafe] []) . postContent . entityVal
  let getId = fromSqlKey . entityKey
  let getTimestamp = formatTime defaultTimeLocale "%d %B %Y" . postTimestamp . entityVal
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

      .post__time {
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
          document.location = "@{HomeR}?id=" + $(this).data("post")
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
                #{preEscapedToMarkup (getHtml post)}
    |]