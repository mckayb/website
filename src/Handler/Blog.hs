{-# LANGUAGE QuasiQuotes #-}

module Handler.Blog where

import Import
import qualified CMarkGFM
import qualified Database.Persist.Sql as Sql (fromSqlKey)
import qualified Helpers.Database as Database
import qualified Data.Text as Text

getPostContent :: Entity Post -> Text
getPostContent = CMarkGFM.commonmarkToHtml [CMarkGFM.optSafe] [] . postContent . entityVal

getTimestamp :: Entity Post -> Text
getTimestamp = Text.pack . formatTime defaultTimeLocale "%d %B %Y" . postTimestamp . entityVal

getPostTitle :: Entity Post -> Text
getPostTitle = postTitle . entityVal

takeWhileOneMore :: (a -> Bool) -> [a] -> [a]
takeWhileOneMore p = foldr (\x ys -> if p x then x:ys else [x]) []

takeUntilFirstParagraphInc :: [Text] -> [Text]
takeUntilFirstParagraphInc = takeWhileOneMore (not . Text.isPrefixOf "<p>")

getPostTeaser :: Entity Post -> Text
getPostTeaser = Text.unlines . takeUntilFirstParagraphInc . Text.lines . getPostContent

getBlogR :: Handler Html
getBlogR = do
  posts <- Database.getPosts
  let getId = Sql.fromSqlKey . entityKey
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
        <div .row>
          <div .col-md-12>Coming soon!

      $else
        $forall post <- posts
          <div .row .row-bottom-border>
            <div .col-md-12>
              <article .post>
                <h1 .post__title>
                  <a href="@{BlogR}post/#{getId post}">#{getPostTitle post}
                <div .post__date .text-muted>
                  #{getTimestamp post}
                <div .post__content>
                  #{preEscapedToMarkup (getPostTeaser post)}
    |]

getBlogPostR :: Key Post -> Handler Html
getBlogPostR a = do
  post <- Database.getPost a
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