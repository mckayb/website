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
  let markdownToHtml = (commonmarkToHtml [optSafe] []) . postContent . entityVal
  let htmlPosts = fmap (\x -> (markdownToHtml x, (fromSqlKey . entityKey) x)) posts
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

      .row--border {
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
      $if (null htmlPosts)
        <div>Coming soon!

      $else
        $forall htmlPost <- htmlPosts
          <div .row .row--border>
            <div .col-md-12>
              <article .post data-post=#{snd htmlPost}>
                #{preEscapedToMarkup (fst htmlPost)}
    |]