{-# LANGUAGE QuasiQuotes #-}

module Handler.Blog where

import Import
import qualified CMarkGFM
import qualified Database.Persist.Sql as Sql (fromSqlKey)
import qualified Helpers.Database as Database
import qualified Data.Text as Text
import qualified Helpers.Theme as Theme

getPostContent :: Entity Post -> Text
getPostContent = CMarkGFM.commonmarkToHtml [] [] . postContent . entityVal

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

      .post h1,
      .post h2,
      .post h3 {
        margin-top: 0;
      }

      .post h1 {
        font-size: 24px;
      }

      .post h2 {
        font-size: 21px;
      }
    |]
    [whamlet|
      $if (null posts)
        <div .row>
          <div .col-md-12>Coming soon!

      $else
        $forall post <- posts
          <article .post>
            <h1 .post__title>
              <a href="@{BlogR}post/#{getId post}">#{getPostTitle post}
            <div .post__date .text-muted>
              #{getTimestamp post}
            <div .post__content>
              #{preEscapedToMarkup (getPostTeaser post)}
    |]

getBlogPostR :: Key Post -> Handler Html
getBlogPostR postId = do
  post <- Database.getPost postId
  case post of
    Just post' -> defaultLayout $ do
      toWidget [lucius|
        .blog-post .blog-post__header {
          border-bottom: 1px solid #{Theme.sidebarColor Theme.colorScheme};
          margin-bottom: 5vh;
        }
      |]
      [whamlet|
        <article .blog-post>
          <section .blog-post__header>
            <h1>#{getPostTitle post'}
            <div .blog-post__time .text-muted>#{getTimestamp post'}
          <section .blog-post__body>
            #{preEscapedToMarkup (getPostContent post')}
      |]
    Nothing -> notFound