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

getPostTitle :: Entity Post -> Text
getPostTitle = postTitle . entityVal

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

takeUntilFirstParagraphInc :: [Text] -> [Text]
takeUntilFirstParagraphInc = takeWhileOneMore (not . T.isPrefixOf "<p>")

getPostTeaser :: Entity Post -> Text
getPostTeaser = T.unlines . takeUntilFirstParagraphInc . T.lines . getPostContent

getBlogR :: Handler Html
getBlogR = do
  posts <- getPosts
  let getId = fromSqlKey . entityKey
  defaultLayout $ do
    setTitle "Blog"
    toWidget [lucius|
      .post .post__content {
        margin-bottom: 21px;
      }

      .post .post__date {
        margin-bottom: 15px;
      }

      .post .post__title a {
        color: black;
      }

      .post h1,
      .post h2,
      .post h3 {
        margin-top: 0;
      }

      .row-bottom-border {
        border-bottom: 1px solid lightgray;
        margin-left: 0;
        margin-right: 0;
        margin-bottom: 21px;
      }
    |]
    [whamlet|
      $if (null posts)
        <div>Coming soon!

      $else
        $forall post <- posts
          <div .row .row-bottom-border>
            <div .col-md-12>
              <article .post>
                <h1 .post__title>
                  <a href="@{BlogR}/#{getId post}">#{getPostTitle post}
                <div .post__date .text-muted>
                  #{getTimestamp post}
                <div .post__content>
                  #{preEscapedToMarkup (getPostTeaser post)}
    |]

getBlogPostR :: Key Post -> Handler Html
getBlogPostR a = do
  post <- getPost a
  case post of
    Just post' -> defaultLayout $ do
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
          <h1>#{getPostTitle post'}
          #{preEscapedToMarkup (getPostContent post')}
      |]
    Nothing -> notFound