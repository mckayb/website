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
        background:linear-gradient(transparent 75%, white);
      }

      .post {
        max-height: 200px;
        overflow: hidden;
      }
    |]
    [whamlet|
      $if (null htmlPosts)
        <div>Coming soon!

      $else
        $forall htmlPost <- htmlPosts
          <article .post value=#{snd htmlPost}>
            #{preEscapedToMarkup (fst htmlPost)}
    |]